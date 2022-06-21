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
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.MaudPreferences;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.foreign.MemorySession;

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
		  "linear*q", "linear*q^2", "linear*q^4", "log10*q", "log10*q^2", "log10*q^4"};
  public static String[] xplotMode = {"Default", "d-space", "Q space", "Energy"};
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

    toolsMenu.add(menuitem = new JMenuItem("Peaks location"));
    menuitem.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        peaksLocationFrame();
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

	  toolsMenu.add(menuitem = new JMenuItem("Run FPSM on data"));
	  menuitem.addActionListener(new ActionListener() {
		  public void actionPerformed(ActionEvent e) {
			  String result = phaseIdentificationByFPSM();
			  if (result != null) {
				  System.out.println(result);
			  }
		  }
	  });

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

  public void exportPeaksDicvol91() {
  }

  public void setCustomPeakList(Phase aphase) {
  }

	public String exportOriginalDataFPSM() {
		return null;
	}

	public String phaseIdentificationByFPSM() {
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
/*		try {
			com.radiographema.fpsm.fpsm_h.fpsmSearch();
		} catch (Exception e) {
		}*/

		return results;
	}

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

  static int scaleModeInt = -1;

  public static int getScaleMode() {
    if (scaleModeInt == -1)
      checkScaleMode();
    return scaleModeInt;
  }

  public static void checkScaleMode() {
    String scaleString = MaudPreferences.getPref(principalJFrame.plotScale, PlotDataFile.plotMode[0]);
    int nmode = plotMode.length;
    for (int i = 0; i < nmode; i++)
      if (plotMode[i].equalsIgnoreCase(scaleString)) {
        scaleModeInt = i;
        break;
      }
  }

//  static int backSubtract = -1;

  public static boolean subtractBackground() {
//    if (backSubtract == -1)
//      checkBackgroundSubtraction();
    return plotNoBkgDefault;
  }

  public static void checkBackgroundSubtraction() {
    plotNoBkgDefault = MaudPreferences.getBoolean(plotNoBkg, plotNoBkgDefault);
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
