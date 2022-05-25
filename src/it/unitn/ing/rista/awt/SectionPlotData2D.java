/*
 * @(#)SectionPlotData2D.java created Feb 24, 2014 Caen
 *
 * Copyright (c) 1996-2014 Luca Lutterotti All Rights Reserved.
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
		import java.awt.event.ActionEvent;
		import java.awt.event.ActionListener;
		import java.io.BufferedWriter;
		import java.io.IOException;
		import java.util.Vector;

		import it.unitn.ing.jgraph.*;
		import it.unitn.ing.rista.diffr.*;
		import it.unitn.ing.rista.interfaces.Peak;
		import it.unitn.ing.rista.util.*;

		import javax.swing.*;


/**
 * The DifferencePlot2D is a class
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.3 $, $Date: 2004/08/30 09:36:23 $
 * @since JDK1.1
 */

public class SectionPlotData2D extends myJFrame {

	Graph2D graph = null;
	CopyPrintPanel fullGraphPanel = null;

	public static int defaultMarker = 4;
	public static int markerNumber = defaultMarker;
	public static double markerScale = 0.5;
	public static Color markerColor = Color.blue;
	public static int XaxisTitleFontScale = 16;
	public static int XaxisLabelFontScale = 16;
	public static Color XaxisTitleColor = Color.blue;
	public static int YaxisTitleFontScale = 16;
	public static int YaxisLabelFontScale = 16;
	public static Color YaxisTitleColor = Color.blue;
	public static int PhasesFontScale = 14;

/*	public static String[] plotMode = {"sqrt", "linear", "log10", "log10*q", "log10*q^2", "log10*q^4"};
	public static String[] xplotMode = {"Default", "d-space", "Q space", "Energy"};
	public static final String xaxisModePref = "plot.XaxisMode";
	public static final String plotNoBkg = "plot.backgroundSubtraction";
	public static boolean plotNoBkgDefault = false;
	public static final String plotCalIntensity = "plot.calibrateIntensity";
	public static boolean plotCalIntensityDefault = false;*/
	public static String axisFont = MaudPreferences.getPref("plot.axisFont", "TimesRoman");
	public static String labelFont = MaudPreferences.getPref("plot.labelFont", "Helvetica");
	public static boolean blackAndWhite = MaudPreferences.getBoolean("plot.black&white", false);

	SpectrumPlotPanel thePlotPanel = null;
	private DiffrDataFile[] datafile = null;

	String title = null;
	DataSet[] datam = null;
	DataSet[] dataFitm = null;
	boolean debug = false;
	private Axis xaxis = null;
	private Axis yaxis = null;
	private int np;

	public SectionPlotData2D(Frame parent) {
		super(parent);

		frameWLabel = "plot.frameWidth";
		frameHLabel = "plot.frameHeight";
		defaultFrameW = 600;
		defaultFrameH = 400;
		setOwnSize = true;
		framePositionX = "plot.framePositionX";
		framePositionY = "plot.framePositionY";
		defaultFramePositionX = 10;
		defaultFramePositionY = 20;
		setOwnPosition = true;
		axisFont = MaudPreferences.getPref("plot.axisFont", "TimesRoman");
		labelFont = MaudPreferences.getPref("plot.labelFont", "Helvetica");
		blackAndWhite = MaudPreferences.getBoolean("plot.black&white", false);

		if (blackAndWhite) {
			markerColor = Color.black;
			XaxisTitleColor = Color.black;
			YaxisTitleColor = Color.black;
		} else {
			markerColor = Color.blue;
			XaxisTitleColor = Color.blue;
			YaxisTitleColor = Color.blue;
		}

	}

	public SectionPlotData2D(Frame parent, DiffrDataFile[] afile, String title) {

		this(parent);

		this.title = title;

		createDefaultMenuBar();

		Container p1 = getContentPane();
		p1.setLayout(new BorderLayout());
		p1.setBackground(Color.white);
		datafile = afile;
//		thePlotPanel = createGraph(afile);
		if (thePlotPanel != null)
			p1.add(thePlotPanel, BorderLayout.CENTER);
		setComponentToPrint(thePlotPanel.getComponentToPrint());

	}

	public JMenuBar createDefaultMenuBar() {
		JMenuBar amenubar = super.createDefaultMenuBar();
		amenubar.add(createPlottingOptionMenu());
		amenubar.add(createToolsMenu());
		return amenubar;
	}

	public JMenu createPlottingOptionMenu() {

		JMenuItem menuitem = null;

		JMenu optionsMenu = new JMenu("Plotting");
		optionsMenu.setMnemonic('p');
		optionsMenu.add(menuitem = new JMenuItem("Options"));
/*		menuitem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				showOptionsDialog();
			}
		});*/

		return optionsMenu;
	}

	public JMenu createToolsMenu() {

		JMenuItem menuitem = null;

		JMenu toolsMenu = new JMenu("Tools");
		toolsMenu.setMnemonic('t');

		toolsMenu.add(menuitem = new JMenuItem("Export experimental data"));
		menuitem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				exportOriginalData();
			}
		});

		toolsMenu.add(menuitem = new JMenuItem("Export computed data"));
		menuitem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				exportComputedData();
			}
		});

		toolsMenu.add(menuitem = new JMenuItem("Export experimental/computed data"));
		menuitem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				exportExperimentalComputedData();
			}
		});

		return toolsMenu;
	}

	public void exportOriginalData() {

		if (thePlotPanel.datafile == null || thePlotPanel.datafile[0] == null)
			return;

		String filename = Utility.openFileDialog(this, "Save as CIF...",
				FileDialog.SAVE, thePlotPanel.datafile[0].getFilePar().getDirectory(), null, "put a name.cif");
		if (filename == null)
			return;

		String[] folderAndName = Misc.getFolderandName(filename);

		String folder = folderAndName[0];
		filename = folderAndName[1];

		if (!filename.endsWith(".cif"))
			filename = filename + ".cif";

		if (filename != null) {

			BufferedWriter output = Misc.getWriter(folder, filename);
			try {
				int nPoints = thePlotPanel.datafile[0].computeDataNumber();
				output.write("_pd_meas_number_of_points " + Integer.toString(nPoints));
				output.newLine();
//        if (datafile[0].originalNotCalibrated)
//          output.write("_riet_meas_datafile_calibrated false");
//        else
				output.write("_riet_meas_datafile_calibrated true");
				output.newLine();
				output.newLine();
				output.write("loop_");
				output.newLine();
				output.write(DiffrDataFile.CIFXcoord2T);
				output.newLine();
				output.write("_pd_meas_intensity_total");
				output.newLine();
				int starting = thePlotPanel.datafile[0].startingindex;
				int ending = thePlotPanel.datafile[0].finalindex;
				int step = 1;
				if (thePlotPanel.datafile[0].getXData(ending - 1) < thePlotPanel.datafile[0].getXData(starting)) {
					starting = thePlotPanel.datafile[0].finalindex - 1;
					ending = thePlotPanel.datafile[0].startingindex - 1;
					step = -1;
				}
				for (int i = starting; i != ending; i+=step) {
					double intens = thePlotPanel.datafile[0].getYData(i);
					double xcoorddata = thePlotPanel.datafile[0].getXData(i);
					output.write(" " + Fmt.format(xcoorddata) + " " + Fmt.format(intens));
					output.newLine();
				}
			} catch (IOException io) {
			}
			try {
				output.close();
			} catch (IOException io) {
			}
		}
	}

	public void exportComputedData() {

		if (thePlotPanel.datafile == null || thePlotPanel.datafile[0] == null)
			return;

		String filename = Utility.openFileDialog(this, "Save as CIF...",
				FileDialog.SAVE, thePlotPanel.datafile[0].getFilePar().getDirectory(), null, "put a name.cif");
		if (filename == null)
			return;

		String[] folderAndName = Misc.getFolderandName(filename);

		String folder = folderAndName[0];
		filename = folderAndName[1];

		if (!filename.endsWith(".cif"))
			filename = filename + ".cif";

		if (filename != null) {

			boolean addStatisticalError = thePlotPanel.datafile[0].getFilePar().addStatisticalError;

			BufferedWriter output = Misc.getWriter(folder, filename);
			try {
				int nPoints = thePlotPanel.datafile[0].computeDataNumber();
				output.write("_pd_meas_number_of_points " + Integer.toString(nPoints));
				output.newLine();
//        if (datafile[0].originalNotCalibrated)
//          output.write("_riet_meas_datafile_calibrated false");
//        else
				output.write("_riet_meas_datafile_calibrated true");
				output.newLine();
				output.newLine();
				output.write("loop_");
				output.newLine();
				output.write(DiffrDataFile.CIFXcoord2T);
				output.newLine();
				output.write(DiffrDataFile.intensityCalcCIFstring);
				output.newLine();
				for (int i = thePlotPanel.datafile[0].startingindex; i < thePlotPanel.datafile[0].finalindex; i++) {
					double intens = thePlotPanel.datafile[0].getFit(i);
					if (addStatisticalError)
						intens += MoreMath.getGaussianNoise(intens); //getIntensityRandomError(intens);
					if (intens < 0)
						intens = 0.0;
					double xcoorddata = 0.0;
//          if (datafile[0].originalNotCalibrated)
//            xcoorddata = datafile[0].getXDataOriginal(i);
//          else
					xcoorddata = thePlotPanel.datafile[0].getXData(i);
					output.write(" " + Fmt.format(xcoorddata) + " " + Fmt.format(intens));
					output.newLine();
				}
			} catch (IOException io) {
			}
			try {
				output.close();
			} catch (IOException io) {
			}
		}
	}

	public void exportExperimentalComputedData() {

		if (thePlotPanel.datafile == null || thePlotPanel.datafile[0] == null)
			return;

		String filename = Utility.openFileDialog(this, "Save as CIF...",
				FileDialog.SAVE, thePlotPanel.datafile[0].getFilePar().getDirectory(), null, "put a name.cif");
		if (filename == null)
			return;

		String[] folderAndName = Misc.getFolderandName(filename);

		String folder = folderAndName[0];
		filename = folderAndName[1];
		int numberphases = thePlotPanel.datafile[0].getFilePar().getActiveSample().phasesNumber();

		if (!filename.endsWith(".cif"))
			filename = filename + ".cif";

		if (filename != null) {

			BufferedWriter output = Misc.getWriter(folder, filename);
			try {
				int nPoints = thePlotPanel.datafile[0].computeDataNumber();
				output.write("_pd_meas_number_of_points " + Integer.toString(nPoints));
				output.newLine();
//        if (datafile[0].originalNotCalibrated)
//          output.write("_riet_meas_datafile_calibrated false");
//        else
				output.write("_riet_meas_datafile_calibrated true");
				output.newLine();
				output.newLine();
				output.write("# On the following loop you will have:");
				output.newLine();
				output.write("#  2theta/d coordinate   experimental intensity  calculated intensity  background");
				for (int j = 0; j < numberphases; j++)
					output.write("  intensity " + thePlotPanel.datafile[0].getFilePar().getActiveSample().getPhase(j));
				output.newLine();
				output.newLine();
				output.write("loop_");
				output.newLine();
				output.write(DiffrDataFile.CIFXcoord2T);
				output.newLine();
				output.write("_pd_proc_intensity_total");
				output.newLine();
				output.write(DiffrDataFile.intensityCalcCIFstring);
				output.newLine();
				output.write("_pd_proc_intensity_bkg_calc");
				output.newLine();
				output.write("_pd_proc_intensity_weight");
				output.newLine();
				for (int j = 0; j < numberphases; j++) {
					output.write(DiffrDataFile.intensityCalcCIFstring);
					output.newLine();
				}
				for (int i = thePlotPanel.datafile[0].startingindex; i < thePlotPanel.datafile[0].finalindex; i++) {
					double intensE = thePlotPanel.datafile[0].getYData(i);
					double intens = thePlotPanel.datafile[0].getFit(i);
					double xcoorddata = 0.0;
//          if (datafile[0].originalNotCalibrated)
//            xcoorddata = datafile[0].getXDataOriginal(i);
//          else
					xcoorddata = thePlotPanel.datafile[0].getXData(i);
					output.write(" " + Fmt.format(xcoorddata) + " " + Fmt.format(intensE) + " " + Fmt.format(intens));
					output.write(" " + Fmt.format(thePlotPanel.datafile[0].getBkgFit(i)));
					output.write(" " + Fmt.format(thePlotPanel.datafile[0].getWeight(i)));
					for (int j = 0; j < numberphases; j++)
						output.write(" " + Fmt.format(thePlotPanel.datafile[0].getPhaseFit(i, j)));
					output.newLine();
				}
			} catch (IOException io) {
			}
			try {
				output.close();
			} catch (IOException io) {
			}
		}
	}



	public void createGraph(DiffrDataFile[] afile) {

/*		getContentPane().setBackground(Color.white);

		datafile = afile;
		boolean markExcludedRegion = MaudPreferences.getBoolean("excludedRegion.setZeroForPlot", true);

		if (datafile != null) {

			FilePar filepar = datafile[0].getFilePar();

			int numberphases = filepar.getActiveSample().phasesNumber();
			Phase[] phaselist = new Phase[numberphases];
			for (int i = 0; i < numberphases; i++)
				phaselist[i] = filepar.getActiveSample().getPhase(i);

			Graph2D residuals;
			G2Dint positions;
			int i;
			int j;

			setTitle(title);
			Container p1 = getContentPane();
			p1.setLayout(new BorderLayout());
			fullGraphPanel = new CopyPrintPanelNoBkg();
			fullGraphPanel.setBackground(Color.white);
			p1.add(fullGraphPanel, BorderLayout.CENTER);

			GridBagLayout gridbag = new GridBagLayout();
			GridBagConstraints c = new GridBagConstraints();

			fullGraphPanel.setLayout(gridbag);


//      Create the Graph instance and modify the default behaviour

			graph = new G2Dint();
			G2Dint lgraph = (G2Dint) graph;

			c.fill = GridBagConstraints.BOTH;
			c.weighty = 1.0;
			c.weightx = 1.0;
			c.gridwidth = GridBagConstraints.REMAINDER;

			gridbag.setConstraints(graph, c);
			fullGraphPanel.add(graph);

			lgraph.drawzero = false;
			lgraph.drawgrid = false;
			lgraph.borderTop = 30;
			lgraph.borderBottom = 1;


//      Load a file containing Marker definitions

			Markers marker = null;
			try {
				marker = new Markers(Misc.getFilesResource("marker.txt"));
			} catch (java.io.IOException ioe) {
				ioe.printStackTrace();
			}
			if (marker != null)
				lgraph.setMarkers(marker);

			double maxY = 0.0;
			double offset = 10.0;

			datam = new DataSet[datafile.length];
			dataFitm = null;

			for (int sn = 0; sn < datafile.length; sn++) {
				Color spectraColor;
				if (blackAndWhite)
					spectraColor = Color.black;
				else
					spectraColor = getPastelColor(sn);

				np = datafile[sn].finalindex - datafile[sn].startingindex;

				double data[] = new double[2 * np];
				int mode = checkScaleModeX();
				for (i = j = 0; i < np; i++, j += 2) {
					data[j] = datafile[sn].getXDataForPlot(i + datafile[sn].startingindex, mode);
					data[j + 1] = datafile[sn].getYSqrtData(i + datafile[sn].startingindex) + offset * sn;

					if (sn == 0)
						if (data[j + 1] > maxY)
							maxY = data[j + 1];
				}

				if (sn == 0)
					offset = maxY * offsetControl;

				if (debug)
					System.out.println("Data loaded for spectra # " + sn);

				datam[sn] = lgraph.loadDataSet(data, np);
				if (debug)
					System.out.println("Data added to graph");
				if (markerNumber < 0 && datafile[sn].hasfit())
					markerNumber = defaultMarker;
				if (markerNumber < 0) {
					datam[sn].linestyle = 1;
					datam[sn].linecolor = spectraColor;
				} else {
					datam[sn].linestyle = 0;
					datam[sn].marker = markerNumber;
					datam[sn].markerscale = markerScale;
					datam[sn].markercolor = spectraColor;
				}
//        	datam[sn].legend(200,100,datafile.getAxisXLegend());
//        	datam[sn].legendColor(Color.black);
//						datam[sn].legend(data[np-2] * 1.005, offset * (0.1 + sn), datafile[sn].toXRDcatString());
//        		datam[sn].legendFont(new Font("TimesRoman",Font.PLAIN,PhasesFontScale));
//        		datam[sn].legendColor(getPastelColor(sn));

				if (datafile[sn].hasfit()) {
					if (dataFitm == null)
						dataFitm = new DataSet[datafile.length];
					for (i = j = 0; i < np; i++, j += 2) {
						if (datafile[sn].xInsideRange(datafile[sn].getXData(i + datafile[sn].startingindex)) || !markExcludedRegion)
							data[j + 1] = datafile[sn].getFitSqrtData(i + datafile[sn].startingindex) + offset * sn;
						else
							data[j + 1] = Double.NaN;
					}
					if (debug)
						System.out.println("Fit computed");
					dataFitm[sn] = lgraph.loadDataSet(data, np);
					if (debug)
						System.out.println("Fit added to graph");
					dataFitm[sn].linecolor = spectraColor;

				}

//      Attach data sets to the Xaxis

//          System.out.println("Attaching X-axis....");

				if (xaxis == null) {
					xaxis = lgraph.createXAxis();
					if (datafile.length > 1) {
						xaxis.setTitleText(datafile[sn].getAxisXLegend());
						xaxis.setTitleFont(new Font(axisFont, Font.BOLD, XaxisTitleFontScale));
						xaxis.setLabelFont(new Font(labelFont, Font.PLAIN, XaxisLabelFontScale));
						xaxis.setTitleColor(XaxisTitleColor);
					} else {
						xaxis.drawLabel = false;
						xaxis.drawTitle = false;
					}
				}
				xaxis.attachDataSet(datam[sn]);
				if (datafile[sn].hasfit()) {
					xaxis.attachDataSet(dataFitm[sn]);
				}
				if (debug)
					System.out.println("Data attached to x");

//      Attach the first data set to the Left Axis

//          System.out.println("Attaching Y-axis....");

				if (yaxis == null)
					yaxis = lgraph.createYAxis();
				if (debug)
					System.out.println("Data attached to y");
				yaxis.attachDataSet(datam[sn]);
				if (datafile[sn].hasfit()) {
					yaxis.attachDataSet(dataFitm[sn]);
					if (debug)
						System.out.println("Fit attached");
				}
			}
			yaxis.setTitleText(DiffrDataFile.getAxisYLegend());
			yaxis.setTitleFont(new Font(axisFont, Font.BOLD, YaxisTitleFontScale));
			yaxis.setLabelFont(new Font(labelFont, Font.PLAIN, YaxisLabelFontScale));
			yaxis.setTitleColor(YaxisTitleColor);

			if (datafile[0].hasfit()) {


//      prepare the positions box

				DataFileSet adataset = datafile[0].getDataFileSet();
				int numberofPeaks = adataset.getNumberofPeaks();
				Vector<Peak> peaklist = adataset.getPeakList();

				int numberradiation = adataset.getInstrument().getRadiationType().getLinesCount();

				if (numberradiation == 0)
					numberradiation = 1;
				int numberofRefl = numberofPeaks;

				if (numberofRefl > 0) {

					positions = new G2Dint();

					lgraph.addComponent(positions);

					c.weighty = 0.04 * (numberphases + 1);
					c.gridwidth = GridBagConstraints.REMAINDER;

					gridbag.setConstraints(positions, c);
					fullGraphPanel.add(positions);

					positions.drawzero = false;
					positions.drawgrid = false;
					positions.frame = false;
					positions.borderTop = 1;
					positions.borderBottom = 1;

//      Load a file containing Marker definitions


					if (marker != null)
						positions.setMarkers(marker);

					double[] datapeak = new double[2 * numberofRefl];

					int dimension = numberradiation;
					if (numberphases > dimension)
						dimension = numberphases;

					datap = new PeakSet[dimension];

					int mode = checkScaleModeX();

					for (int ijn = 0; ijn < numberradiation; ijn++) {
						double wave = adataset.getInstrument().getRadiationType().getRadiationWavelength(ijn);
						for (i = j = 0; i < numberofPeaks; i++, j += 2) {
							Phase tmpphase = peaklist.elementAt(i).getPhase();
							int phaseindex = 0;
							for (int ij = 0; ij < numberphases; ij++)
								if (tmpphase == phaselist[ij])
									phaseindex = ij;
							// todo modify for more peaks par pattern
							double pos = adataset.getActiveDataFile(0).getPositions(tmpphase)[ijn][peaklist.elementAt(i).getOrderPosition()][0];
							datapeak[j] = datafile[0].convertXDataForPlot(pos, wave, mode);

							datapeak[j + 1] = (double) (phaseindex + 1);
						}
						datap[ijn] = positions.loadPeakSet(datapeak, numberofRefl);
						datap[ijn].linestyle = 0;
						datap[ijn].marker = 9;
						double mscale = 2.0 - ijn;
						if (mscale <= 0.01)
							mscale = 0.5;
						datap[ijn].markerscale = mscale;

//            int index = 0;
//            if (ijn > 0)
//              index = 1;
//        		 		datap[ijn].markercolor = getPastelColor(ijn);

					}

					datapeak = new double[2];
					for (int ijn = numberradiation; ijn < numberphases; ijn++) {
						datapeak[0] = 0.0;
						datapeak[1] = 1.0;
						datap[ijn] = positions.loadPeakSet(datapeak, 1);
						datap[ijn].linestyle = 0;
						datap[ijn].marker = 9;
						datap[ijn].markerscale = 0.1;
//        			  datap[ijn].markercolor = getPastelColor(ijn);
					}
					Color spectracolor;
					for (int ij = 0; ij < numberphases; ij++) {
						double ypos = (double) (ij + 1);
						datap[ij].legend(1, ypos, phaselist[ij].toXRDcatString());
						datap[ij].legendFont(new Font(labelFont, Font.PLAIN, PhasesFontScale));
						if (blackAndWhite)
							spectracolor = Color.black;
						else
							spectracolor = getPastelColor(ij);
						datap[ij].legendColor(spectracolor);
						datap[ij].markercolor = spectracolor;
					}

//          System.out.println("Data loaded");


//      Attach data sets to the Xaxis

//          System.out.println("Attaching X-axis....");

					Axis xaxisp = positions.createXAxis();
					xaxisp.drawLine = false;
					xaxisp.drawLabel = false;
					xaxisp.drawTitle = false;
					xaxisp.referenceAxis = xaxis;
					for (int ijn = 0; ijn < dimension; ijn++)
						xaxisp.attachDataSet(datap[ijn]);


//      Attach the first data set to the Left Axis

//          System.out.println("Attaching Y-axis....");

					Axis yaxisp = positions.createYAxis();
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
					yaxisp.minimum = 0.5;
					yaxisp.maximum = ((double) numberphases) + 0.5;

				}


//      prepare the residuals box


				residuals = new Graph2D();

				lgraph.addComponent(residuals);

				if (datafile.length > 1)
					c.weighty = 0.0;
				else
					c.weighty = 0.3;

				c.gridwidth = GridBagConstraints.REMAINDER;

				gridbag.setConstraints(residuals, c);
				fullGraphPanel.add(residuals);

				residuals.drawzero = false;
				residuals.drawgrid = false;
				residuals.borderTop = 0;
				residuals.borderBottom = 10;

//      Load a file containing Marker definitions


				if (marker != null)
					residuals.setMarkers(marker);

				np = datafile[0].finalindex - datafile[0].startingindex;

				double data[] = new double[2 * np];
				int mode = checkScaleModeX();
				for (i = j = 0; i < np; i++, j += 2) {
					data[j] = (double) datafile[0].getXDataForPlot(i + datafile[0].startingindex, mode);
					if (datafile[0].xInsideRange(datafile[0].getXData(i + datafile[0].startingindex)) || !markExcludedRegion)
						data[j + 1] = datafile[0].getFitSqrtData(i + datafile[0].startingindex) -
								datafile[0].getYSqrtData(i + datafile[0].startingindex);
					else
						data[j + 1] = Double.NaN;
				}
				if (debug)
					System.out.println("Residual loaded");
				DataSet datar = residuals.loadDataSet(data, np);
				if (debug)
					System.out.println("Residual added");


//      Attach data sets to the Xaxis

//          System.out.println("Attaching X-axis....");

				Axis xaxisr = residuals.createAxis(Axis.BOTTOM);
				xaxisr.referenceAxis = xaxis;
				xaxisr.attachDataSet(datar);
				if (debug)
					System.out.println("Residual attached");
				xaxisr.setTitleText(datafile[0].getAxisXLegend());
				xaxisr.setTitleFont(new Font(axisFont, Font.BOLD, XaxisTitleFontScale));
				xaxisr.setLabelFont(new Font(labelFont, Font.PLAIN, XaxisLabelFontScale));
				xaxisr.setTitleColor(XaxisTitleColor);

//      Attach the first data set to the Left Axis

//          System.out.println("Attaching Y-axis....");

				Axis yaxisr = residuals.createAxis(Axis.LEFT);
				yaxisr.drawLine = false;
				yaxisr.drawLabel = false;
				yaxisr.drawTitle = false;
				yaxisr.referenceAxisWidth = yaxis;
				yaxisr.referenceAxisScale = yaxis;
				yaxisr.attachDataSet(datar);
				yaxisr.setTitleText(" ");
				yaxisr.setTitleFont(new Font(axisFont, Font.BOLD, YaxisTitleFontScale));
				yaxisr.setLabelFont(new Font(labelFont, Font.PLAIN, YaxisLabelFontScale));
				yaxisr.setTitleColor(getBackground());

			} else {
				xaxis.setTitleText(datafile[0].getAxisXLegend());
				xaxis.setTitleFont(new Font(axisFont, Font.BOLD, XaxisTitleFontScale));
				xaxis.setLabelFont(new Font(labelFont, Font.PLAIN, XaxisLabelFontScale));
				xaxis.setTitleColor(XaxisTitleColor);
				lgraph.borderBottom = 10;
				xaxis.drawLabel = true;
				xaxis.drawTitle = true;
			}
//					System.out.println("Component to print " + c1);

		} else
			getContentPane().add("South", new Label("No datafile selected!"));

		if (debug)
			System.out.println("Graph completed");
		setComponentToPrint(fullGraphPanel);
		*/
	}

	public static Color[] PastelColor = {Color.red, new Color(0, 102, 0), Color.blue,
			Color.magenta, Color.green, Color.orange,
			Color.cyan, Color.pink, Color.black,
			new Color(102, 51, 0), new Color(153, 0, 153), Color.yellow,
			new Color(102, 102, 0), new Color(0, 102, 102)
	};

	public static Color getPastelColor(int index) {
		int maxNumber = PastelColor.length;
		while (index >= maxNumber)
			index -= maxNumber;
		return PastelColor[index];
	}

	public void showNewFrame() {
		setVisible(false);

		getContentPane().removeAll();
		datam = null;
		dataFitm = null;
//    datar = null;
		xaxis = null;
		yaxis = null;
		if (graph != null)
			if (graph instanceof G2Dint)
				((G2Dint) graph).dispose();
		graph = null;

		createGraph(datafile);
		getContentPane().invalidate();

		getContentPane().validate();

		setVisible(true);
	}

	public void updatePlot() {
/*		int i, j;

		G2Dint lgraph = (G2Dint) graph;

		double[] trange = lgraph.getRanges();
//    double[] prange = positions.getRanges();

		np = datafile[0].finalindex - datafile[0].startingindex;

//    double data[] = new double[2 * np];

		FilePar filepar = datafile[0].getFilePar();

		int numberphases = filepar.getActiveSample().phasesNumber();
		Phase[] phaselist = new Phase[numberphases];
		double maxY = 0.0;
		double offset = 10.0;
		boolean markExcludedRegion = MaudPreferences.getBoolean("excludedRegion.setZeroForPlot", true);

		try {
			int mode = checkScaleModeX();
			for (i = 0; i < numberphases; i++)
				phaselist[i] = filepar.getActiveSample().getPhase(i);
			for (int sn = 0; sn < datafile.length; sn++) {
//        Color spectraColor = getPastelColor(sn);

				np = datafile[sn].finalindex - datafile[sn].startingindex;

				double data[] = new double[2 * np];
				for (i = j = 0; i < np; i++, j += 2) {
					data[j] = (double) datafile[sn].getXDataForPlot(i + datafile[sn].startingindex, mode);
					data[j + 1] = datafile[sn].getYSqrtData(i + datafile[sn].startingindex) + offset * sn;

					if (sn == 0)
						if (data[j + 1] > maxY)
							maxY = data[j + 1];
				}

				datam[sn].deleteData();
				datam[sn].append(data, np);

				if (sn == 0)
					offset = maxY * offsetControl;

				if (datafile[sn].hasfit()) {

					for (i = j = 0; i < np; i++, j += 2) {
						if (datafile[sn].xInsideRange(datafile[sn].getXData(i + datafile[sn].startingindex)) ||
								!markExcludedRegion)
							data[j + 1] = datafile[sn].getFitSqrtData(i + datafile[sn].startingindex) + offset * sn;
						else
							data[j + 1] = Double.NaN;
					}
					dataFitm[sn].deleteData();
					dataFitm[sn].append(data, np);

				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		lgraph.updateDataAndPaint(trange);
		*/
	}

/*  public void updatePlot() {

  	int i, j;

    G2Dint lgraph = (G2Dint) graph;

  	double[] trange = lgraph.getRanges();
//    double[] prange = positions.getRanges();

		np = datafile[0].finalindex - datafile[0].startingindex;

    double data[] = new double[2 * np];

    FilePar filepar = (FilePar) datafile[0].getFilePar();

    int numberphases = filepar.phasesNumber();
    Phase[] phaselist = new Phase[numberphases];
    for (i = 0; i < numberphases; i++)
      phaselist[i] = (Phase) filepar.getphase(i);

		if (datafile[0].hasfit()) {
			try {

        int mode = checkScaleModeX();
      	for(i=j=0; i<np; i++,j+=2) {
      		data[j] = (double) datafile[0].getXDataForPlot(i + datafile[0].startingindex, mode);
        	data[j+1] = datafile[0].getFitSqrtData(i + datafile[0].startingindex);
      	}
    		data1.deleteData();
    		data1.append(data, np);

    		for(i=j=0; i<np; i++,j+=2) {
      		data[j+1] = datafile[0].getFitSqrtData(i + datafile[0].startingindex) -
      								datafile[0].getYSqrtData(i + datafile[0].startingindex);
    		}
    		datar.deleteData();
    		datar.append(data, np);

        DataFileSet adataset = datafile[0].getDataFileSet();
        int numberofPeaks = adataset.getNumberofPeaks();
        Peak[] peaklist = adataset.getPeakList();

        int numberradiation = adataset.getLinesCount();

        if (numberradiation == 0)
          numberradiation = 1;
        int numberofRefl = numberofPeaks / numberradiation;
        double[] datapeak = new double[2*numberofRefl];

        if (numberofRefl > 0) {
          for (int ijn = 0; ijn < numberradiation; ijn++) {
            double wave = adataset.getRadiationWavelength(ijn);
            for(i=j=0; i<numberofPeaks; i++,j+=2) {
              Phase tmpphase = peaklist[i].getPhase();
              int phaseindex = 0;
              for (int ij = 0; ij < numberphases; ij++)
                if (tmpphase == phaselist[ij])
                  phaseindex = ij;
              double[] pos = datafile[0].getFinalPosition(peaklist[i]);
              datapeak[j] = datafile[0].convertXDataForPlot(pos[ijn], wave, mode);

              datapeak[j+1] = (double) (phaseindex + 1);
            }
            datap[ijn].deleteData();
            datap[ijn].linestyle = 0;
            datap[ijn].marker    = 9;
            double mscale = 2.0 - ijn;
            if (mscale <= 0.01)
              mscale = 0.5;
            datap[ijn].markerscale = mscale;
            datap[ijn].append(datapeak, numberofRefl);
          }
          datapeak = new double[2];
          for (int ijn = numberradiation; ijn < numberphases; ijn++) {
            datapeak[0] = 0.0;
            datapeak[1] = 1.0;
            datap[ijn].linestyle = 0;
            datap[ijn].marker    = 9;
            datap[ijn].markerscale = 0.1;
            datap[ijn].markercolor = getPastelColor(ijn);
            datap[ijn].append(datapeak, 1);
          }
        }
    	} catch (Exception e) {}

      if (yaxisp != null) {
        yaxisp.minimum = 0.5;
        yaxisp.maximum = ((double) numberphases) + 0.5;
      }
    }

//    positions.updateDataAndPaint(prange);
    lgraph.updateDataAndPaint(trange);

//    residuals.updateDataAndPaint();

  }*/

/*	public void showOptionsDialog() {
//    showOptionsDialog();
		JOptionsDialog adialog = new JMultiPlottingOptionsD(this, null);
		adialog.setVisible(true);
	}

	class JMultiPlottingOptionsD extends JPlottingOptionsD {

		JTextField SpectraIntensityOffsetTF = null;

		public JMultiPlottingOptionsD(Frame parent, XRDcat obj) {

			super(parent, obj);

			principalPanel.add(new JLabel("Spectra intensity offset: "));
			SpectraIntensityOffsetTF = new JTextField(Constants.FLOAT_FIELD);
			SpectraIntensityOffsetTF.setToolTipText(
					"Control the intensity separation between spectra as a factor of maximum intensity");
			principalPanel.add(SpectraIntensityOffsetTF);

			JMultiPlottingOptionsD.this.setTitle("Plotting options");
			initParameters();
			JMultiPlottingOptionsD.this.pack();
		}

		public void initParameters() {
			super.initParameters();
			if (SpectraIntensityOffsetTF != null)
				SpectraIntensityOffsetTF.setText(Double.toString(offsetControl));
		}

		public void retrieveParameters() {
			offsetControl = Double.valueOf(SpectraIntensityOffsetTF.getText()).doubleValue();

			super.retrieveParameters();
		}
	}*/

}
