package ij.plugin.filter;

import java.awt.*;
import java.awt.event.*;
import java.text.*;

import ij.*;
import ij.gui.*;
import ij.process.*;
import ij.plugin.filter.PlugInFilter;
import ij.text.*;
import ij.measure.*;
import it.unitn.ing.rista.util.*;

public class MultiSpectraGeneration extends OvalSpectraSelection {

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

  public void run(ImageProcessor ip) {
//    checkExistingRoi();
//    imp.getCalibration().setUnit("mm");
    (new Thread() {
      public void run() {
        double radius = MaudPreferences.getDouble("camera.defaultRadius", 194.71964);
        LaueOvalStepRoi roi = new LaueOvalStepRoi(imp, radius);
	      Roi usableRoi = imp.getRoi();
        imp.setRoi(roi);
        if (getParameters((LaueOvalStepRoi) imp.getRoi()))
//		OvalSpectraPlot p = new OvalSpectraPlot(imp, radius);
          new SpectraProfilePlot(imp, false).createWindow(imp, usableRoi);
      }
    }).start();
  }

  boolean getParameters(LaueOvalStepRoi roi) {
    MultiRoiChangeDialog param = new MultiRoiChangeDialog("Choose the integration lines",
            IJ.getInstance(), roi);

    param.addNumericField("Camera radius ("+imp.getCalibration().getUnit()+")", roi.getRadius(), 2);
    param.addNumericField("Center X ("+imp.getCalibration().getUnit()+")", roi.getX(), 2);
    param.addNumericField("Center Y ("+imp.getCalibration().getUnit()+")", roi.getY(), 2);
    param.addNumericField("Tracker radius (deg 2Theta)", roi.getCircle(), 2);
/*    param.addNumericField("Selection height ("+imp.getCalibration().getUnit()+")", MaudPreferences.getDouble(
            "camera.defaultHeight", 100.0), 2);*/
    param.addNumericField("Diffr. cone interval (deg)", MaudPreferences.getDouble(
            "camera.defaultDiffractionConeInterval", 5.0), 2);
    param.addNumericField("Max cone angle (+/- in deg)", MaudPreferences.getDouble(
            "camera.defaultDiffractionConeMaxAngle", 70.0), 2);
    param.addNumericField("Omega angle (in deg)", MaudPreferences.getDouble(
            "camera.defaultOmegaAngle", 15.0), 2);
    param.addNumericField("Chi angle (in deg)", MaudPreferences.getDouble(
            "camera.defaultChiAngle", 0.0), 2);
    param.addNumericField("Phi angle (in deg)", MaudPreferences.getDouble(
            "camera.defaultPhiAngle", 0.0), 2);
    param.addCheckbox("2-Theta angles calibrated", MaudPreferences.getBoolean("anglesCalibration.imageToSpectra", false));
//		param.addChoice("Analysis mode", modes, modes[mode]);
//		param.addCheckbox("Show Hotspots", hotspots);

    param.showDialog();
    if (!param.wasCanceled()) {
//      radius = param.getNextNumber();
//			mode = param.getNextChoiceIndex();
//			hotspots = param.getNextBoolean();
      param.resetIndices();
      double radius = param.getNextNumber();
      MaudPreferences.setPref("camera.defaultRadius", radius);
      double x = param.getNextNumber();
      double y = param.getNextNumber();
      double diameter = param.getNextNumber();
//      double cameraHeight = param.getNextNumber();
      double coneAngleStep = param.getNextNumber();
      double coneAngleMax = param.getNextNumber();
      double omega = param.getNextNumber();
      double chi = param.getNextNumber();
      double phi = param.getNextNumber();
      boolean calibrated = param.getNextBoolean();
      MaudPreferences.setPref("camera.startingPointX", x);
      MaudPreferences.setPref("camera.startingPointY", y);
//      MaudPreferences.setPref("camera.defaultHeight", cameraHeight);
      MaudPreferences.setPref("camera.defaultDiffractionConeInterval", coneAngleStep);
      MaudPreferences.setPref("camera.defaultDiffractionConeMaxAngle", coneAngleMax);
      MaudPreferences.setPref("camera.defaultOmegaAngle", omega);
      MaudPreferences.setPref("camera.defaultChiAngle", chi);
      MaudPreferences.setPref("camera.defaultPhiAngle", phi);
      MaudPreferences.setPref("anglesCalibration.imageToSpectra", calibrated);
	    MaudPreferences.setPref("d19Detector.defaultEtaConeInterval", coneAngleStep);
//	    MaudPreferences.setPref("d19Detector.defaultDiffractionStepAngle", 0.05);
	    MaudPreferences.setPref("d19Detector.defaultEtaConeAngleMax", coneAngleMax);
      roi.setRadius(radius);
      roi.setStartingPoint(x, y);
      roi.setCircle(diameter);
//      roi.setSelHeight(cameraHeight);
      roi.setConeInterval(coneAngleStep);
      roi.setConeAngleMax(coneAngleMax);
      roi.setOmega(omega);
      roi.setChi(chi);
      roi.setPhi(phi);
      if (calibrated)
        roi.setCalibrated();
      else
        roi.setUncalibrated();
      roi.updateSelection();
      return true;
    } else
      return false;
  }

  void showAbout() {
    IJ.showMessage("About OvalSpectraSelection", " Transform Laue circles to\n" +
            " spectra from a curved 2D detector like an Image Plate\n" +
            " in a Debye-Scherrer type camera (need radius of the camera)." +
            " Spectra are generated for different cone intervals");
  }

  class MultiRoiChangeDialog extends MaudGenericDialog {

    LaueOvalStepRoi roi = null;
    Button apply = null;

    public MultiRoiChangeDialog(String title, Frame parent, LaueOvalStepRoi roi) {
      super(title, parent);
      this.roi = roi;
    }

    /** Displays this dialog box. */
    public void showDialog() {
      apply = new Button("Apply");
      super.showDialog(apply);
    }

    public void actionPerformed(ActionEvent e) {
      if (e.getSource() == apply) {
        resetIndices();
        double radius = getNextNumber();
        MaudPreferences.setPref("camera.defaultRadius", radius);
        double x = getNextNumber();
        double y = getNextNumber();
        double diameter = getNextNumber();
//        double cameraHeight = getNextNumber();
        double coneAngleStep = getNextNumber();
        double coneAngleMax = getNextNumber();
        double omega = getNextNumber();
        double chi = getNextNumber();
        double phi = getNextNumber();
        boolean calibrated = getNextBoolean();
        MaudPreferences.setPref("camera.startingPointX", x);
        MaudPreferences.setPref("camera.startingPointY", y);
//        MaudPreferences.setPref("camera.defaultHeight", cameraHeight);
        MaudPreferences.setPref("camera.defaultDiffractionConeInterval", coneAngleStep);
        MaudPreferences.setPref("camera.defaultDiffractionConeMaxAngle", coneAngleMax);
	      MaudPreferences.setPref("d19Detector.defaultEtaConeInterval", coneAngleStep);
//	    MaudPreferences.setPref("D19Detector.defaultDiffractionStepAngle", 0.05);
	      MaudPreferences.setPref("d19Detector.defaultEtaConeAngleMax", coneAngleMax);
        MaudPreferences.setPref("camera.defaultOmegaAngle", omega);
        MaudPreferences.setPref("camera.defaultChiAngle", chi);
        MaudPreferences.setPref("camera.defaultPhiAngle", phi);
        MaudPreferences.setPref("anglesCalibration.imageToSpectra", calibrated);
        roi.setRadius(radius);
        roi.setStartingPoint(x, y);
        roi.setCircle(diameter);
//        roi.setSelHeight(cameraHeight);
        roi.setConeInterval(coneAngleStep);
        roi.setConeAngleMax(coneAngleMax);
        roi.setOmega(omega);
        roi.setChi(chi);
        roi.setPhi(phi);
        if (calibrated)
          roi.setCalibrated();
        else
          roi.setUncalibrated();
        roi.updateSelection();
      } else
        super.actionPerformed(e);
    }

  }

}

