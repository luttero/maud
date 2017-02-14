package ij.plugin.filter;

import ij.*;
import ij.process.*;
import ij.gui.*;

import java.awt.event.*;

/** Implements the "Plot Profile" command. */
public class SpectraProfiler implements PlugInFilter {

  ImagePlus imp;

  public int setup(String arg, ImagePlus imp) {
    if (arg.equals("set")) {
      doOptions();
      return DONE;
    }
    this.imp = imp;
    return DOES_ALL + NO_UNDO + NO_CHANGES + ROI_REQUIRED;
  }

  public void run(ImageProcessor ip) {
    boolean averageHorizontally = IJ.altKeyDown();
    new SpectraProfilePlot(imp, averageHorizontally).createWindow(imp);
  }

  public void doOptions() {
    double ymin = SpectraProfilePlot.getFixedMin();
    double ymax = SpectraProfilePlot.getFixedMax();
    boolean fixedScale = ymin != 0.0 || ymax != 0.0;
    boolean wasFixedScale = fixedScale;

    GenericDialog gd = new GenericDialog("Spectra Plot Options", IJ.getInstance());
    gd.addNumericField("Y Min:", ymin, 2);
    gd.addNumericField("Y Max:", ymax, 2);
    gd.addCheckbox("Fixed Y-axis Scale", fixedScale);
    gd.addCheckbox("Do Not Save X-Values", !SpectraPlotWindow.saveXValues);
    gd.addCheckbox("Auto-close", SpectraPlotWindow.autoClose);
    gd.showDialog();
    if (gd.wasCanceled())
      return;
    ymin = gd.getNextNumber();
    ymax = gd.getNextNumber();
    fixedScale = gd.getNextBoolean();
    SpectraPlotWindow.saveXValues = !gd.getNextBoolean();
    SpectraPlotWindow.autoClose = gd.getNextBoolean();
    if (!fixedScale && !wasFixedScale && (ymin != 0.0 || ymax != 0.0))
      fixedScale = true;
    if (!fixedScale) {
      ymin = 0.0;
      ymax = 0.0;
    }
    SpectraProfilePlot.setMinAndMax(ymin, ymax);
  }

}

