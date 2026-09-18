/*
 * @(#)PlotDataFile.java created 01/01/1997 Mesiano
 *
 * Copyright (c) 1997 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;
//import java.lang.foreign.MemorySession;

/**
 * The PlotDataFile is a class
 *
 * @version $Revision: 1.11 $, $Date: 2006/11/10 09:32:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class PlotDataFile extends GraphFrame {

//      Graph2D graph;
//  private DataSet data1 = null;
//  private Axis xaxis = null;
//  private Axis yaxis = null;
//  private int np;
//      URL markerURL;

//  private DiffrDataFile[] datafile = null;

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

  public static String[] plotMode = {"sqrt", "linear", "log10", "sqrt*q", "sqrt*q^2", "sqrt*q^4",
		  "linear*q", "linear*q^2", "linear*q^4", "log10*q", "log10*q^2", "log10*q^4", "sqrt/sqrt(q)", "sqrt/q"};
  public static String[] xplotMode = {"Default", "d-space", "Q space", "Energy", "Original", "Channels"};
  public static final String xaxisModePref = "plot.XaxisMode";
  public static final String plotNoBkg = "plot.backgroundSubtraction";
  public static boolean plotNoBkgDefault = false;
  public static final String plotCalIntensity = "plot.calibrateIntensity";
	public static boolean plotCalIntensityDefault = false;
	public static final String plotLPIntensity = "plot.calibrateForLorentz-Polarization";
  public static boolean plotLPIntensityDefault = false;
  public static String axisFont = MaudPreferences.getPref("plot.axisFont", "TimesRoman");
  public static String labelFont = MaudPreferences.getPref("plot.labelFont", "Helvetica");
  public static boolean blackAndWhite = MaudPreferences.getBoolean("plot.black&white", false);
  public static boolean plotBackground = MaudPreferences.getBoolean("plot.plotBackground", false);

  public PlotDataFile(Frame parent) {

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

  public PlotDataFile(Frame parent, DiffrDataFile[] afile) {
    this(parent);
    createDefaultMenuBar();
    createGraph(afile);
  }

  public void createGraph(DiffrDataFile[] afile) {
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
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        showOptionsDialog();
      }
    });

    return optionsMenu;
  }

  public JMenu createToolsMenu() {

    JMenuItem menuitem = null;

    JMenu toolsMenu = new JMenu("Tools");
    toolsMenu.setMnemonic('t');

    toolsMenu.add(menuitem = new JMenuItem("Fourier smoothing"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        fourierSmoothing();
      }
    });

	  toolsMenu.add(menuitem = new JMenuItem("Savitzky-Golay smoothing"));
	  menuitem.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent e) {
			  smoothing();
		  }
	  });

	  toolsMenu.add(menuitem = new JMenuItem("Background subtraction"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        backgroundSubtraction();
      }
    });

    toolsMenu.add(menuitem = new JMenuItem("Kalpha2 stripping"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        kalpha2Stripping();
      }
    });

    toolsMenu.add(menuitem = new JMenuItem("Peaks location (FT)"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        peaksLocationFrame();
      }
    });

    toolsMenu.add(menuitem = new JMenuItem("Peaks location (manual)"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        peaksLocationManualFrame();
      }
    });

    toolsMenu.add(menuitem = new JMenuItem("Reset"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        resetStartingPoint();
      }
    });

    toolsMenu.add(menuitem = new JMenuItem("Export peaks (dicvol91)"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        exportPeaksDicvol91();
      }
    });

//    toolsMenu.add(menuitem = new JMenuItem("Add peaks to"));
    JMenu amenu = new JMenu("Add peaks to");
    toolsMenu.add(amenu);
    JMenuItem submenu = null;
    Sample asample = getFileParent().getActiveSample();
    for (int ip = 0; ip < asample.phasesNumber(); ip++) {
      final Phase aphase = asample.getPhase(ip);
      submenu = new JMenuItem(aphase.getLabel());
      amenu.add(submenu);
      submenu.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          setCustomPeakList(aphase);
        }
      });
    }

	  toolsMenu.add(menuitem = new JMenuItem("Export for FPSM"));
	  menuitem.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent e) {
			  exportOriginalDataFPSM();
		  }
	  });

/*	  toolsMenu.add(menuitem = new JMenuItem("Run FPSM on data"));
	  menuitem.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent e) {
			  String result = phaseIdentificationByFPSM();
			  if (result != null) {
				  System.out.println(result);
			  }
		  }
	  });*/

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

    toolsMenu.add(menuitem = new JMenuItem("Export from plot"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        exportForCalibrationData();
      }
    });

    toolsMenu.add(menuitem = new JMenuItem("Export for pdf analysis"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        exportForPDF();
      }
    });

    toolsMenu.add(menuitem = new JMenuItem("Export fit for pdf plotting"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        exportComputedPDF();
      }
    });

    toolsMenu.add(menuitem = new JMenuItem("Edit interpolated background points"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        editInterpolatedBackgroundPoints();
      }
    });

	  toolsMenu.add(menuitem = new JMenuItem("Fourier transform (PDF)"));
	  menuitem.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent e) {
			  fourierTransform();
		  }
	  });

	  toolsMenu.add(menuitem = new JMenuItem("Inverse Fourier transform (PDF)"));
	  menuitem.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent e) {
			  inverseFourierTransform();
		  }
	  });

	  toolsMenu.add(menuitem = new JMenuItem("Inverse Fourier transform (Reflectivity)"));
	  menuitem.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent e) {
			  inverseReflectivityFourierTransform();
		  }
	  });

	  return toolsMenu;
  }

	public void resetStartingPoint() {
  }

  public void smoothing() {
  }

  public void fourierSmoothing() {
  }

	public void fourierTransform() {
	}

	public void inverseFourierTransform() {
	}

	public void inverseReflectivityFourierTransform() {
	}

	public void backgroundSubtraction() {
  }

  public void kalpha2Stripping() {
  }

  public void peaksLocationFrame() {
  }

  public void peaksLocationManualFrame() {
  }

  public void exportPeaksDicvol91() {
  }

  public void setCustomPeakList(Phase aphase) {
  }

	public String exportOriginalDataFPSM() {
		return null;
	}

/*	public String phaseIdentificationByFPSM() {
		String results = null;
		String filename = exportOriginalDataFPSM();
		if (filename != null && filename.length() > 4) {
			String analysis = filename.substring(0, filename.length() - 3) + ".json";
			String database = Utility.openFileDialog(this, "Select database of structures (.sqlite, .json",
					FileDialog.LOAD, Constants.documentsDirectory, null, Constants.documentsDirectory);
			if (database != null) {
				try {
					var session0 = MemorySession.openConfined();
					var filenameS = session0.allocateUtf8String(filename);
					var session1 = MemorySession.openConfined();
					var databaseS = session1.allocateUtf8String(database);
					var session2 = MemorySession.openConfined();
					var analysisS = session2.allocateUtf8String(analysis);
					java.lang.foreign.MemoryAddress addr = com.radiographema.fpsm.fpsm_h.searchbyfpsm(
							filenameS.address(), databaseS.address(), analysisS.address());
					results = addr.getUtf8String(0);
				} catch (Exception e) {
				}
			}
		}
//		try {
//			com.radiographema.fpsm.fpsm_h.fpsmSearch();
//		} catch (Exception e) {
//		}

		return results;
	}*/

	public void exportOriginalData() {
/*
    if (datafile == null || datafile[0] == null)
      return;

    String filename = Misc.openFileDialog(this, "Save as CIF...",
            FileDialog.SAVE, datafile[0].getFilePar().getDirectory(), null, "put a name.cif");
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
        int nPoints = datafile[0].computeDataNumber();
        output.write("_pd_meas_number_of_points " + Integer.toXRDcatString(nPoints));
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
        output.write(DiffrDataFile.intensityCIFstring);
        output.newLine();
        for (int i = datafile[0].startingindex; i < datafile[0].finalindex; i++) {
          double intens = datafile[0].getYData(i);
          double xcoorddata = 0.0;
//          if (datafile[0].originalNotCalibrated)
//            xcoorddata = datafile[0].getXDataOriginal(i);
//          else
          xcoorddata = datafile[0].getXData(i);
          output.write(" " + Fmt.format(xcoorddata) + " " + Fmt.format(intens));
          output.newLine();
        }
      } catch (IOException io) {
      }
      try {
        output.close();
      } catch (IOException io) {
      }
    }*/
  }

  public void exportForCalibrationData() {}

  public void exportForPDF() {}

  public void exportComputedPDF() {}

  public void exportComputedData() {

/*    if (datafile == null || datafile[0] == null)
      return;

    String filename = Misc.openFileDialog(this, "Save as CIF...",
            FileDialog.SAVE, datafile[0].getFilePar().getDirectory(), null, "put a name.cif");
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
        int nPoints = datafile[0].computeDataNumber();
        output.write("_pd_meas_number_of_points " + Integer.toXRDcatString(nPoints));
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
        output.write(DiffrDataFile.intensityCIFstring);
        output.newLine();
        for (int i = datafile[0].startingindex; i < datafile[0].finalindex; i++) {
          double intens = datafile[0].getFit(i);
          double xcoorddata = 0.0;
//          if (datafile[0].originalNotCalibrated)
//            xcoorddata = datafile[0].getXDataOriginal(i);
//          else
          xcoorddata = datafile[0].getXData(i);
          output.write(" " + Fmt.format(xcoorddata) + " " + Fmt.format(intens));
          output.newLine();
        }
      } catch (IOException io) {
      }
      try {
        output.close();
      } catch (IOException io) {
      }
    }*/
  }

  public void exportExperimentalComputedData() {
  }

  public void editInterpolatedBackgroundPoints() {

  }

  public void dispose() {
/*    if (graph != null)
      if (graph instanceof G2Dint)
        ((G2Dint) graph).dispose();
    graph = null;*/
    super.dispose();
  }

  public void showNewFrame() {
/*    setVisible(false);

    getContentPane().removeAll();
    data1 = null;
    xaxis = null;
    yaxis = null;
    if (graph != null)
      if (graph instanceof G2Dint)
        ((G2Dint) graph).dispose();
    graph = null;

    createGraph(datafile);
    
    getContentPane().invalidate();

    getContentPane().validate();

    setVisible(true);*/
  }

  // Utilities to change X, Y scales

  public static double getIntensity(DiffrDataFile datafile, double x, int index) {
    if (datafile.isInsideHoles(x))
      return Double.NaN;
      return datafile.getBasicInterpolatedIntensity(x, index);
  }

  public static double getFitIntensity(DiffrDataFile datafile, double x, int index) {
    if (datafile.isInsideHoles(x))
      return Double.NaN;
    return datafile.getBasicInterpolatedFit(x, index);
  }

  public static double getPhaseFitIntensity(DiffrDataFile datafile, double x, int index, int nphase) {
    if (datafile.isInsideHoles(x))
      return Double.NaN;
    return datafile.getBasicInterpolatedPhaseFit(x, index, nphase);
  }

  public static double getBackground(DiffrDataFile datafile, double x, int index) {
    if (datafile.isInsideHoles(x))
      return Double.NaN;
    return datafile.getBasicInterpolatedBackground(x, index);
  }

  public static double getIntensityCalibration(DiffrDataFile datafile, double x, int index) {
    if (datafile.isInsideHoles(x))
      return Double.NaN;
    return datafile.computeIntensityCalibration(x, index);
  }

  public static double getIntensityLPCalibration(DiffrDataFile datafile, double x, int index) {
    if (datafile.isInsideHoles(x))
      return Double.NaN;
    return datafile.getDataFileSet().getInstrument().LorentzPolarization(datafile, datafile.getFilePar().getActiveSample(), x);
  }

  public static double getScaledIntensity(DiffrDataFile datafile, double intensity, double x, int modeY) {
    int sign = 1;
    switch (modeY) {
      case 1:
        return intensity;
      case 2:
        if (intensity <= 0.0)
          intensity = 0.0;
        else
          intensity = Math.log(intensity) * Constants.log10Conv;
        return intensity;
      case 3:
        intensity *= datafile.getXInQ(x);
        if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.sqrt(intensity) * sign;
      case 4:
        intensity *= MoreMath.pow(datafile.getXInQ(x), 2);
        if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.sqrt(intensity) * sign;
      case 5:
        intensity *= MoreMath.pow(datafile.getXInQ(x), 4);
        if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.sqrt(intensity) * sign;
      case 6:
        intensity *= datafile.getXInQ(x);
        return intensity;
      case 7:
        intensity *= MoreMath.pow(datafile.getXInQ(x), 2);
        return intensity;
      case 8:
        intensity *= MoreMath.pow(datafile.getXInQ(x), 4);
        return intensity;
      case 9:
        intensity *= datafile.getXInQ(x);
        if (intensity == 0.0)
          return 0.0;
        else if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.log(intensity) * Constants.log10Conv * sign;
      case 10:
        intensity *= MoreMath.pow(datafile.getXInQ(x), 2);
        if (intensity == 0.0)
          return 0.0;
        else if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.log(intensity) * Constants.log10Conv * sign;
      case 11:
        intensity *= MoreMath.pow(datafile.getXInQ(x), 4);
        if (intensity == 0.0)
          return 0.0;
        else if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.log(intensity) * Constants.log10Conv * sign;
      case 12:
        double factor = Math.sqrt(Math.abs(datafile.getXInQ(x)));
        if (factor > 0)
          intensity /= factor;
        return intensity;
      case 13:
        double factor1 = datafile.getXInQ(x);
        if (factor1 > 0)
          intensity /= factor1;
        return intensity;
      case 0:
      default: {
        if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.sqrt(intensity) * sign;
      }
    }

  }

    public static double getScaledX(DiffrDataFile datafile, double x, int modeX) {
      return datafile.getXDataForPlot(x, modeX);
    }

  public static String getAxisXLegend(boolean calibrated, boolean dspacingbase, boolean energyDispersive) {
    if (!calibrated)
      return "Uncalibrated";
    int mode = checkScaleModeX();
    if (mode == 2)
      return "Q [Angstrom{^-1}]";
    if (mode == 4)
      return "Uncalibrated (original)";
    if (mode == 5)
      return "Channel";
    if (dspacingbase || mode == 1)
      return "d [Angstrom]";
    if (energyDispersive || mode == 3)
      return "Energy [eV]";
    return "2-Theta [degrees]";
  }

  public static String getAxisXLegendNoUnit(boolean calibrated, boolean dspacingbase, boolean energyDispersive) {
    if (!calibrated)
      return "Uncalibrated";
    int mode = checkScaleModeX();
    if (mode == 2)
      return "Q";
    if (mode == 4)
      return "Uncalibrated";
    if (mode == 5)
      return "Channel";
    if (dspacingbase || mode == 1)
      return "d";
    if (energyDispersive || mode == 3)
      return "Energy";
    return "2-Theta";
  }

  public static String getAxisXLegendUnit(boolean calibrated, boolean dspacingbase, boolean energyDispersive) {
    if (!calibrated)
      return "";
    int mode = checkScaleModeX();
    if (mode == 2)
      return "Angstrom^-1";
    if (mode == 4)
      return "original";
    if (mode == 5)
      return "number";
    if (dspacingbase || mode == 1)
      return "Angstrom";
    if (energyDispersive || mode == 3)
      return "eV";
    return "degrees";
  }

  public static String getAxisYLegend() {
    switch (getScaleMode()) {
      case 1:
        return "Intensity [Count]";
      case 2:
        return "Log10(Intensity) [Log10(Count)]";
      case 3:
        return "Intensity{^1/2} * Q";
      case 4:
        return "Intensity{^1/2} * Q^2";
      case 5:
        return "Intensity{^1/2} * Q^4";
      case 6:
        return "Intensity * Q";
      case 7:
        return "Intensity * Q^2";
      case 8:
        return "Intensity * Q^4";
      case 9:
        return "Log10(Intensity) * Q";
      case 10:
        return "Log10(Intensity) * Q^2";
      case 11:
        return "Log10(Intensity) * Q^4";
      case 12:
        return "Intensity{^1/2} / Q^1/2";
      case 13:
        return "Intensity{^1/2} / Q";
      case 14:
      default: {
        return "Intensity{^1/2} [Count{^1/2}]";
      }
    }

  }

/*  public static String getAxisYLegend2D() {
    switch (PlotDataFile.getScaleMode()) {
      case 1:
        return "Intensity (Count)";
      case 2:
        return "Log10[Intensity] (Log10[Count])";
      case 3:
        return "Log10(Intensity) * Q";
      case 4:
        return "Log10(Intensity) * Q^2";
      case 5:
        return "Log10(Intensity) * Q^4";
      case 0:
      default: {
        return "Intensity{^1/2} (Count{^1/2})";
      }
    }

  }*/

/*
  public static double getInterpolatedYSqrtIntensity(double xvalue, double expT, double expT2, int mode) {
    xvalue = revertXDataForPlot(xvalue, mode);
    if (isInsideHoles(xvalue))
      return Double.NaN;
    if (PlotDataFile.subtractBackground())
      return getValueScaled(getInterpolatedIntensity(xvalue, expT, expT2) -
          getInterpolatedBackground(xvalue, expT, expT2), getOldNearestPoint(xvalue), PlotDataFile.calibrateIntensity(),
          PlotDataFile.calibrateIntensityForLorentzPolarization(), PlotDataFile.getScaleMode());
    else
      return getValueScaled(getInterpolatedIntensity(xvalue, expT, expT2), getOldNearestPoint(xvalue), PlotDataFile.calibrateIntensity(),
          PlotDataFile.calibrateIntensityForLorentzPolarization(), PlotDataFile.getScaleMode());
  }

  public static double getInterpolatedYSqrtIntensity(double xvalue, int exponent, int mode) {
    return getInterpolatedYSqrtIntensity(xvalue, exponent, mode, PlotDataFile.subtractBackground(),
        PlotDataFile.calibrateIntensity(), PlotDataFile.calibrateIntensityForLorentzPolarization(),
        PlotDataFile.getScaleMode());
  }

  public static double getInterpolatedYSqrtIntensity(double xvalue, int exponent, int mode, boolean subtractBackground,
                                              boolean calibrate, boolean lorentz, int ymode) {
    double xvaluen = revertXDataForPlot(xvalue, mode);
//    System.out.println(xvalue + " " + xvaluen);
    if (isInsideHoles(xvaluen))
      return Double.NaN;
    if (subtractBackground)
      return getValueScaled(getInterpolatedIntensityAt(xvaluen, exponent) -
          getInterpolatedBkgFitAt(xvaluen, exponent), getOldNearestPoint(xvaluen), calibrate, lorentz, ymode);
    else
      return getValueScaled(getInterpolatedIntensityAt(xvaluen, exponent), getOldNearestPoint(xvaluen), calibrate, lorentz, ymode);
  }

  public static double getInterpolatedFitSqrtIntensity(double xvalue, int exponent, int mode, boolean subtractBackground,
                                                boolean calibrate, boolean lorentz, int ymode) {
    xvalue = revertXDataForPlot(xvalue, mode);
    if (isInsideHoles(xvalue))
      return Double.NaN;
    if (subtractBackground)
      return getValueScaled(getInterpolatedFitAt(xvalue, exponent) -
          getInterpolatedBkgFitAt(xvalue, exponent), getOldNearestPoint(xvalue), calibrate, lorentz, ymode);
    else
      return getValueScaled(getInterpolatedFitAt(xvalue, exponent), getOldNearestPoint(xvalue), calibrate, lorentz, ymode);
  }

  public static double getInterpolatedFitSqrtIntensity(double xvalue, int exponent, int mode) {
    xvalue = revertXDataForPlot(xvalue, mode);
    if (isInsideHoles(xvalue))
      return Double.NaN;
    if (PlotDataFile.subtractBackground())
      return getValueScaled(getInterpolatedFitAt(xvalue, exponent) -
          getInterpolatedBkgFitAt(xvalue, exponent), getOldNearestPoint(xvalue));
    else
      return getValueScaled(getInterpolatedFitAt(xvalue, exponent), getOldNearestPoint(xvalue));
  }

  public static double getInterpolatedFitSqrtIntensity(double xvalue, int exponent, int mode, int phase) {
    xvalue = revertXDataForPlot(xvalue, mode);
    if (isInsideHoles(xvalue))
      return Double.NaN;
    if (PlotDataFile.subtractBackground())
      return getValueScaled(getInterpolatedFitAt(xvalue, exponent, phase), getOldNearestPoint(xvalue));
    else
      return getValueScaled(getInterpolatedFitAt(xvalue, exponent, phase) +
          getInterpolatedBkgFitAt(xvalue, exponent), getOldNearestPoint(xvalue));
  }

  public static double getInterpolatedBkgFitSqrtIntensity(double xvalue, int exponent, int mode) {
    xvalue = revertXDataForPlot(xvalue, mode);
    if (isInsideHoles(xvalue))
      return Double.NaN;
    return getValueScaled(getInterpolatedBkgFitAt(xvalue, exponent), getOldNearestPoint(xvalue));
  }

  public static double getYSqrtData(int index, boolean backgroundSubtract) {
    if (backgroundSubtract)
      return getValueScaled(getYData(index) - getBkgFit(index), index);
    else
      return getValueScaled(getYData(index), index);
  }

  public static double getFitSqrtData(int index) {
    if (PlotDataFile.subtractBackground())
      return getValueScaled(getFit(index) - getBkgFit(index), index);
    else
      return getValueScaled(getFit(index), index);
  }

  public static double getFitSqrtData(int index, int phaseIndex) {
    if (PlotDataFile.subtractBackground())
      return getValueScaled(getPhaseFit(index, phaseIndex), index);
    else
      return getValueScaled(getPhaseFit(index, phaseIndex) + getBkgFit(index), index);
  }

  public static double getBkgFitSqrtData(int index) {
    return getValueScaled(getBkgFit(index), index);
  }
*/
  public static void updateDataForPlot(DataFileSet adata) {

    adata.dataForPlot = null;
    adata.datafitForPlot = null;
    adata.backgroundForPlot = null;
    adata.dataphaseForPlot = null;

    DiffrDataFile[] datafile = adata.getActiveDataFiles();
    if (datafile == null || datafile.length == 0)
      return;

    int ylength = datafile.length;
    int startingIndex = datafile[0].startingindex;
    int finalIndex = datafile[0].finalindex;
    double xmin = 1.0E10, xmax = 0.0;
    double stepX = 1.0E10;

    int modeX = checkScaleModeX();
    int modeY = checkScaleMode();
    checkCalibrateIntensity();
    boolean calibInt = calibrateIntensity();
    boolean calibLP = calibrateIntensityForLorentzPolarization();
    boolean bkgSub = checkBackgroundSubtraction();
    double minEnergyKeV = Constants.checkMinimumEnergy();

    FilePar filepar = datafile[0].getFilePar();

    int numberphases = filepar.getActiveSample().phasesNumber();
//      if (!plotPeaks)

    Phase[] phaselist = new Phase[numberphases];
    for (int i = 0; i < numberphases; i++)
      phaselist[i] = filepar.getActiveSample().getPhase(i);

    for (int is1 = 0; is1 < ylength; is1++) {
      int xlength = datafile[is1].finalindex - datafile[is1].startingindex - 1;
      double x1 = datafile[is1].getXData(datafile[is1].startingindex);
      double x2 = datafile[is1].getXData(datafile[is1].finalindex - 1);
      double lstepX = Math.abs((x2 - x1) / xlength);
      if (lstepX < stepX)
        stepX = lstepX;
      if (xmin > x1)
        xmin = x1;
      if (xmax < x2)
        xmax = x2;
      if (xmin > x2)
        xmin = x2;
      if (xmax < x1)
        xmax = x1;
    }
    int np = (int) Math.abs((xmax - xmin) / stepX) + 1;

    if (np > 0) {
      adata.dataForPlot  = new double[2 * np];
      boolean hasFit = datafile[0].hasfit();
      double b_value = 0;
      if (bkgSub || plotBackground)
        adata.backgroundForPlot = new double[2 * np];
      if (hasFit) {   //   || peaksLocated     todo:  should be in?
        adata.datafitForPlot = new double[2 * np];
      }
      if (adata.datafitForPlot != null) {
        adata.dataphaseForPlot = new double[numberphases][2 * np];
      }
      for (int is1 = 0; is1 < np; is1++) {
        int is2 = is1 * 2;
        adata.dataForPlot[is2] = xmin + is1 * stepX;
        int total = 0;
        int totalFit = 0;
        for (int sn = 0; sn < ylength; sn++) {
          double xstartmin = datafile[sn].getXData(datafile[sn].startingindex);
          double xendmax = datafile[sn].getXData(datafile[sn].finalindex - 1);
          if (xendmax < datafile[sn].getXData(datafile[sn].startingindex))
            xendmax = datafile[sn].getXData(datafile[sn].startingindex);
          if (xstartmin > datafile[sn].getXData(datafile[sn].finalindex - 1))
            xstartmin = datafile[sn].getXData(datafile[sn].finalindex - 1);
          if (adata.dataForPlot[is2] >= xstartmin && adata.dataForPlot[is2] <= xendmax) {
            int index = datafile[sn].getOldNearestPoint(adata.dataForPlot[is2]);
            double value = getIntensity(datafile[sn], adata.dataForPlot[is2], index);
            double f_value = Double.NaN;
            double[] phaseFit = new double[numberphases];
            if (hasFit) {
              f_value = getFitIntensity(datafile[sn], adata.dataForPlot[is2], index);
              for (int ip = 0; ip < numberphases; ip++)
                phaseFit[ip] = getPhaseFitIntensity(datafile[sn], adata.dataForPlot[is2], index, ip);
              if (bkgSub || plotBackground)
                b_value = getBackground(datafile[sn], adata.dataForPlot[is2], index);
            }
            if (calibInt) {
              double calibratingIntensity = getIntensityCalibration(datafile[sn], adata.dataForPlot[is2], index);
              if (calibratingIntensity != 0.0) {
                value /= calibratingIntensity;
                if (hasFit) {
                  if (bkgSub || plotBackground)
                    b_value /= calibratingIntensity;
                  if (!Double.isNaN(f_value))
                    f_value /= calibratingIntensity;
                  for (int ip = 0; ip < numberphases; ip++)
                    phaseFit[ip] /= calibratingIntensity;
                }
              }
            }
            if (calibLP) {
              double calibratingIntensity = getIntensityLPCalibration(datafile[sn], adata.dataForPlot[is2], index);
              if (calibratingIntensity != 0.0) {
                value /= calibratingIntensity;
                if (hasFit) {
                  if (bkgSub || plotBackground)
                    b_value /= calibratingIntensity;
                  if (!Double.isNaN(f_value))
                    f_value /= calibratingIntensity;
                  for (int ip = 0; ip < numberphases; ip++)
                    phaseFit[ip] /= calibratingIntensity;
                }
              }
            }
            if (!Double.isNaN(value)) {
              adata.dataForPlot[is2 + 1] += value;
              total++;
            }
            if (hasFit) {
              if (bkgSub || plotBackground)
              adata.backgroundForPlot[is2 + 1] += b_value;
              if (!Double.isNaN(f_value)) {
                adata.datafitForPlot[is2 + 1] += f_value;
                totalFit++;
              }
              for (int ip = 0; ip < numberphases; ip++)
                adata.dataphaseForPlot[ip][is2 + 1] += phaseFit[ip];
            }
          }
        }
        if (total > 0) {
          adata.dataForPlot[is2 + 1] /= total;
          if (totalFit > 0) {
            adata.datafitForPlot[is2 + 1] /= totalFit;
            for (int ip = 0; ip < numberphases; ip++) {
              adata.dataphaseForPlot[ip][is2 + 1] /= totalFit;
            }
          }
          if (hasFit && bkgSub) {
            adata.backgroundForPlot[is2 + 1] /= total;
            adata.dataForPlot[is2 + 1] -= adata.backgroundForPlot[is2 + 1];
            adata.datafitForPlot[is2 + 1] -= adata.backgroundForPlot[is2 + 1];
          }
          adata.dataForPlot[is2 + 1] = getScaledIntensity(datafile[0], adata.dataForPlot[is2 + 1], adata.dataForPlot[is2], modeY);
          if (hasFit)
            adata.datafitForPlot[is2 + 1] = getScaledIntensity(datafile[0], adata.datafitForPlot[is2 + 1], adata.dataForPlot[is2], modeY);
          if (hasFit && plotBackground)
            adata.backgroundForPlot[is2 + 1] = getScaledIntensity(datafile[0], adata.backgroundForPlot[is2 + 1], adata.dataForPlot[is2], modeY);
          if (hasFit)
            for (int ip = 0; ip < numberphases; ip++)
              adata.dataphaseForPlot[ip][is2 + 1] = getScaledIntensity(datafile[0], adata.dataphaseForPlot[ip][is2 + 1], adata.dataForPlot[is2], modeY);
        }
        adata.dataForPlot[is2] = getScaledX(datafile[0], adata.dataForPlot[is2], modeX);
        if (hasFit)
          adata.datafitForPlot[is2] = adata.dataForPlot[is2];
        if (hasFit && plotBackground)
          adata.backgroundForPlot[is2] = adata.dataForPlot[is2];
        if (hasFit)
          for (int ip = 0; ip < numberphases; ip++)
            adata.dataphaseForPlot[ip][is2] = adata.dataForPlot[is2];
      }


      // minimum maximum range
/*
    for (int is1 = 0; is1 < ylength; is1++) {
      int xlength = datafile[is1].finalindex - datafile[is1].startingindex - 1;
      double x1 = datafile[is1].getXDataForPlot(datafile[is1].startingindex, mode);
      double x2 = datafile[is1].getXDataForPlot(datafile[is1].finalindex - 1, mode);
      double lstepX = Math.abs((x2 - x1) / xlength);
      if (lstepX < stepX)
        stepX = lstepX;
      if (xmin > x1)
        xmin = x1;
      if (xmax < x2)
        xmax = x2;
      if (xmin > x2)
        xmin = x2;
      if (xmax < x1)
        xmax = x1;
    }
    int np = (int) Math.abs((xmax - xmin) / stepX) + 1;

    if (np > 0) {
      adata.dataForPlot = new double[2 * np];
      if (datafile[0].hasfit()) {     // || peaksLocated todo ripristinare
        adata.datafitForPlot = new double[2 * np];
      }
      for (int is1 = 0; is1 < np; is1++) {
        int is2 = is1 * 2;
        adata.dataForPlot[is2] = xmin + is1 * stepX;
        int total = 0;
        int totalFit = 0;

// data

        for (int sn = 0; sn < ylength; sn++) {
          double xstartmin = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
          double xendmax = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
          if (xendmax < datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode))
            xendmax = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
          if (xstartmin > datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode))
            xstartmin = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
          if (adata.dataForPlot[is2] >= xstartmin && adata.dataForPlot[is2] <= xendmax) {
            double value = datafile[sn].getInterpolatedYSqrtIntensity(dataForPlot[is2], 2, mode);
            if (!Double.isNaN(value)) {
              adata.dataForPlot[is2 + 1] += value;
              total++;
            }
          }
        }
        if (total > 0)
          adata.dataForPlot[is2 + 1] /= total;
//        System.out.println("Update: " + is2 + " " + dataForPlot[is2] + " " + dataForPlot[is2 + 1]);

// fit

        if (datafile[0].hasfit()) {
          adata.datafitForPlot[is2] = adata.dataForPlot[is2];

          for (int sn = 0; sn < ylength; sn++) {
            double xstartmin = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
            double xendmax = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
            if (xendmax < datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode))
              xendmax = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
            if (xstartmin > datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode))
              xstartmin = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
            if (adata.datafitForPlot[is2] >= xstartmin && adata.datafitForPlot[is2] <= xendmax) {
              double value = datafile[sn].getInterpolatedFitSqrtIntensity(datafitForPlot[is2], 2, mode);
              if (!Double.isNaN(value)) {
                adata.datafitForPlot[is2 + 1] += value;
                totalFit++;
              }
            }
          }
          if (totalFit > 0)
            adata.datafitForPlot[is2 + 1] /= totalFit;
        }
      }
//			if (datafile[0].hasfit()) {
//				datafitForPlot[1] = datafitForPlot[3]; // Luca: to check, workaround
//				datafitForPlot[np * 2 - 1] = datafitForPlot[np * 2 - 3]; // Luca: to check, workaround
//			}

// Phases fit

      if (adata.datafitForPlot != null) {
        int numberphases = adata.getFilePar().getActiveSample().phasesNumber();
        adata.dataphaseForPlot = new double[numberphases][2 * np];
        for (int s = 0; s < numberphases; s++) {
          Phase phase = adata.getFilePar().getActiveSample().getPhase(s);
          if (phase.plotFit()) {
            int j;
            for (int i = j = 0; i < np; i++, j += 2) {
              int totalFit = 0;
              for (int sn = 0; sn < ylength; sn++) {
                double xstartmin = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
                double xendmax = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
                if (xendmax < datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode))
                  xendmax = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
                if (xstartmin > datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode))
                  xstartmin = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
                if (adata.datafitForPlot[j] >= xstartmin && adata.datafitForPlot[j] < xendmax) {
                  double value = datafile[sn].getInterpolatedFitSqrtIntensity(datafitForPlot[j], 2, mode, s);
                  if (!Double.isNaN(value)) {
                    adata.dataphaseForPlot[s][j + 1] += value;
                    totalFit++;
                  }
                }
              }
              if (totalFit > 0)
                adata.dataphaseForPlot[s][j + 1] /= totalFit;
              adata.dataphaseForPlot[s][j] = adata.datafitForPlot[j];

            }
          }
        }

// Background

        backgroundForPlot = new double[2 * np];
        int j;
        for (int i = j = 0; i < np; i++, j += 2) {
          int totalFit = 0;
          for (int sn = 0; sn < ylength; sn++) {
            double xstartmin = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
            double xendmax = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
            if (xendmax < datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode))
              xendmax = datafile[sn].getXDataForPlot(datafile[sn].startingindex, mode);
            if (xstartmin > datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode))
              xstartmin = datafile[sn].getXDataForPlot(datafile[sn].finalindex - 1, mode);
            if (adata.datafitForPlot[j] >= xstartmin && adata.datafitForPlot[j] <= xendmax) {
              double value = datafile[sn].getInterpolatedBkgFitSqrtIntensity(datafitForPlot[j], 2, mode);
              if (!Double.isNaN(value)) {
                adata.backgroundForPlot[j + 1] += value;
                totalFit++;
              }
            }
          }
          if (totalFit > 0)
            adata.backgroundForPlot[j + 1] /= totalFit;
          adata.backgroundForPlot[j] = adata.datafitForPlot[j];
        }
      }
*/
    }
  }
/*
  public static double getInterpolatedYForPDF(double xvalue) {
    return getInterpolatedYSqrtIntensity(xvalue, 1, 2, true, true,
        false, 1);
  }

  public static double getInterpolatedFitForPDF(double xvalue) {
    return getInterpolatedFitSqrtIntensity(xvalue, 1, 2, true, true,
        false, 1);
  }

  public static double getValueScaled(double intensity, int index, boolean calibrated,
                               boolean lorentz, int mode) {
    int sign = 1;
    double x = getXData(index);
    if (calibrated) {
      double calibratingIntensity = computeIntensityCalibration(index);
      if (calibratingIntensity != 0.0)
        intensity /= calibratingIntensity;
    }
    if (lorentz) {
      Instrument ainstrument = getDataFileSet().getInstrument();
      Sample asample = getFilePar().getActiveSample();
      double calibratingIntensity = ainstrument.LorentzPolarization(this, asample, x);
      if (calibratingIntensity != 0.0)
        intensity /= calibratingIntensity;
    }
    switch (mode) {
      case 1:
        return intensity;
      case 2:
        if (intensity <= 0.0)
          intensity = 0.0;
        else
          intensity = Math.log(intensity) * Constants.log10Conv;
        return intensity;
      case 3:
        intensity *= getXInQ(x);
        if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.sqrt(intensity) * sign;
      case 4:
        intensity *= MoreMath.pow(getXInQ(x), 2);
        if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.sqrt(intensity) * sign;
      case 5:
        intensity *= MoreMath.pow(getXInQ(x), 4);
        if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.sqrt(intensity) * sign;
      case 6:
        intensity *= getXInQ(x);
        return intensity;
      case 7:
        intensity *= MoreMath.pow(getXInQ(x), 2);
        return intensity;
      case 8:
        intensity *= MoreMath.pow(getXInQ(x), 4);
        return intensity;
      case 9:
        intensity *= getXInQ(x);
        if (intensity == 0.0)
          return 0.0;
        else if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.log(intensity) * Constants.log10Conv * sign;
      case 10:
        intensity *= MoreMath.pow(getXInQ(x), 2);
        if (intensity == 0.0)
          return 0.0;
        else if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.log(intensity) * Constants.log10Conv * sign;
      case 11:
        intensity *= MoreMath.pow(getXInQ(x), 4);
        if (intensity == 0.0)
          return 0.0;
        else if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.log(intensity) * Constants.log10Conv * sign;
      case 12:
        double factor = Math.sqrt(Math.abs(getXInQ(x)));
        if (factor > 0)
          intensity /= factor;
        return intensity;
      case 13:
        double factor1 = getXInQ(x);
        if (factor1 > 0)
          intensity /= factor1;
        return intensity;
      case 0:
      default: {
        if (intensity < 0.0) {
          sign = -1;
          intensity = -intensity;
        }
        return Math.sqrt(intensity) * sign;
      }
    }

  }

*/

  static int scaleModeInt = -1;

  public static int getScaleMode() {
    if (scaleModeInt == -1)
      checkScaleMode();
    return scaleModeInt;
  }

  public static int checkScaleMode() {
    String scaleString = MaudPreferences.getPref(principalJFrame.plotScale, plotMode[0]);
    int nmode = plotMode.length;
    for (int i = 0; i < nmode; i++)
      if (plotMode[i].equalsIgnoreCase(scaleString)) {
        scaleModeInt = i;
        break;
      }
    return scaleModeInt;
  }

//  static int backSubtract = -1;

  public static boolean subtractBackground() {
//    if (backSubtract == -1)
//      checkBackgroundSubtraction();
    return plotNoBkgDefault;
  }

  public static boolean checkBackgroundSubtraction() {
    plotNoBkgDefault = MaudPreferences.getBoolean(plotNoBkg, plotNoBkgDefault);
    return plotNoBkgDefault;
  }

//  static int calIntensity = -1;

  public static boolean calibrateIntensity() {
    return plotCalIntensityDefault;
  }

//	static int lpIntensity = -1;

	public static boolean calibrateIntensityForLorentzPolarization() {
		return plotLPIntensityDefault;
	}

	public static void checkCalibrateIntensity() {
    plotCalIntensityDefault = MaudPreferences.getBoolean(plotCalIntensity, plotCalIntensityDefault);
	  plotLPIntensityDefault = MaudPreferences.getBoolean(plotLPIntensity, plotLPIntensityDefault);
  }

  public static int checkScaleModeX() {
    String scaleString = MaudPreferences.getPref(xaxisModePref, xplotMode[0]);
    int nmode = xplotMode.length;
    for (int i = 0; i < nmode; i++)
      if (xplotMode[i].equalsIgnoreCase(scaleString))
        return i;
    return 0;
  }

  public void showOptionsDialog() {
    JOptionsDialog adialog = new JPlottingOptionsD(this, null);
    adialog.setVisible(true);
  }

  class JPlottingOptionsD extends JOptionsDialog {

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

    public JPlottingOptionsD(Frame parent, XRDcat obj) {

      super(parent, obj, "Replot");

      principalPanel.setLayout(new GridLayout(0, 2, 3, 3));

      principalPanel.add(new JLabel(""));
      blackAndWhiteCB = new JCheckBox("Black & White");
      principalPanel.add(blackAndWhiteCB);

      principalPanel.add(new JLabel("Data marker number: "));
      markerNumberTF = new JTextField(Constants.FLOAT_FIELD);
      markerNumberTF.setToolTipText(
              "The shape of the data marker by number (-1 for line plot; see file markers.txt in the maud.jar)");
      principalPanel.add(markerNumberTF);

      principalPanel.add(new JLabel("Data marker scale factor: "));
      markerScaleTF = new JTextField(Constants.FLOAT_FIELD);
      markerScaleTF.setToolTipText("The dimension of the data marker is scaled by this factor");
      principalPanel.add(markerScaleTF);

      principalPanel.add(new JLabel("Axes title font: "));
      axisTitleFontTF = new JTextField(Constants.FLOAT_FIELD);
      axisTitleFontTF.setToolTipText("The font used for the title of the both axes");
      principalPanel.add(axisTitleFontTF);

      principalPanel.add(new JLabel("X axis title font size: "));
      XaxisTitleFontScaleTF = new JTextField(Constants.FLOAT_FIELD);
      XaxisTitleFontScaleTF.setToolTipText("The size of the font used for the title of the X axis");
      principalPanel.add(XaxisTitleFontScaleTF);

      principalPanel.add(new JLabel("Y axis title font size: "));
      YaxisTitleFontScaleTF = new JTextField(Constants.FLOAT_FIELD);
      YaxisTitleFontScaleTF.setToolTipText("The size of the font used for the title of the Y axis");
      principalPanel.add(YaxisTitleFontScaleTF);

      principalPanel.add(new JLabel("Labels font: "));
      LabelFontTF = new JTextField(Constants.FLOAT_FIELD);
      LabelFontTF.setToolTipText("The font used for the labels of the both axes");
      principalPanel.add(LabelFontTF);

      principalPanel.add(new JLabel("X axis label font size: "));
      XaxisLabelFontScaleTF = new JTextField(Constants.FLOAT_FIELD);
      XaxisLabelFontScaleTF.setToolTipText("The size of the font used for the label of the X axis");
      principalPanel.add(XaxisLabelFontScaleTF);

      principalPanel.add(new JLabel("Y axis label font size: "));
      YaxisLabelFontScaleTF = new JTextField(Constants.FLOAT_FIELD);
      YaxisLabelFontScaleTF.setToolTipText("The size of the font used for the label of the Y axis");
      principalPanel.add(YaxisLabelFontScaleTF);

      principalPanel.add(new JLabel("Phase label font size: "));
      PhasesFontScaleTF = new JTextField(Constants.FLOAT_FIELD);
      PhasesFontScaleTF.setToolTipText(
              "The size of the font used for the phase labels (only if a fit is available)");
      principalPanel.add(PhasesFontScaleTF);

      principalPanel.add(new JLabel("Intensity scale mode: "));
      plotModeCB = new JComboBox();
      int nmode = plotMode.length;
      for (int i = 0; i < nmode; i++)
        plotModeCB.addItem(plotMode[i]);
      plotModeCB.setToolTipText("Choose the scale mode for the intensity axis");
      principalPanel.add(plotModeCB);

      principalPanel.add(new JLabel(""));
      subtractBackground = new JCheckBox("Subtract background");
      principalPanel.add(subtractBackground);

      principalPanel.add(new JLabel(""));
      calibrateIntensity = new JCheckBox("Calibration Correction");
      principalPanel.add(calibrateIntensity);

	    principalPanel.add(new JLabel(""));
	    calibrateForLPIntensity = new JCheckBox("Lorentz-Polar. Corr.");
	    principalPanel.add(calibrateForLPIntensity);

	    principalPanel.add(new JLabel("X-axis plot mode: "));
      xplotModeCB = new JComboBox();
      nmode = xplotMode.length;
      for (int i = 0; i < nmode; i++)
        xplotModeCB.addItem(xplotMode[i]);
      xplotModeCB.setToolTipText("Choose the plot mode for the x-axis");
      principalPanel.add(xplotModeCB);

      PlotDataFile.this.setTitle("Plotting options");
      initParameters();
      JPlottingOptionsD.this.pack();
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
      plotModeCB.setSelectedItem(MaudPreferences.getPref(
		      principalJFrame.plotScale, PlotDataFile.plotMode[0]));
      xplotModeCB.setSelectedItem(MaudPreferences.getPref(xaxisModePref, xplotMode[0]));
      subtractBackground.setSelected(
              MaudPreferences.getBoolean(plotNoBkg, PlotDataFile.plotNoBkgDefault));
      calibrateIntensity.setSelected(
              MaudPreferences.getBoolean(plotCalIntensity, PlotDataFile.plotCalIntensityDefault));
	    calibrateForLPIntensity.setSelected(
			    MaudPreferences.getBoolean(plotLPIntensity, PlotDataFile.plotLPIntensityDefault));
      blackAndWhiteCB.setSelected(blackAndWhite);
    }

    public void retrieveParameters() {
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
      MaudPreferences.setPref("plot.black&white", PlotDataFile.blackAndWhite);
      MaudPreferences.setPref(plotCalIntensity, calibrateIntensity.isSelected());
	    MaudPreferences.setPref(plotLPIntensity, calibrateForLPIntensity.isSelected());
      MaudPreferences.setPref(xaxisModePref, xplotModeCB.getSelectedItem().toString());
      checkScaleMode();
      checkBackgroundSubtraction();
      checkCalibrateIntensity();

      showNewFrame();
    }
  }

}
