package ij.plugin.filter;

import ij.IJ;
import ij.ImagePlus;
import ij.gui.*;
import ij.process.ImageProcessor;
import it.unitn.ing.rista.util.MaudPreferences;

import java.awt.*;
import java.awt.event.ActionEvent;

/*
 *  Oval_Profile  takes the image region bounded by an oval region and samples the oval
 *  at N equal angles around the oval. The program generates a ProfilePlot of either:
 *    1) pixel values along the oval.
 *    2) maximum intensity values along N radii from the oval center to oval points.
 *      This measure is called the circumferential profile and is used in Nuclear Medicine
 *      cardiac blood perfusion studies, in which short axis slices of the left
 *      ventricle are roughly doughnut shaped.
 */

public class OvalSpectraSelection implements PlugInFilter {
  protected ImagePlus imp;
//  Roi usableArea = null;
  //  protected double width, height;

//  protected static double defHeight = MaudPreferences.getDouble("camera.defaultHeight", 100.0);

  public int setup(String args, ImagePlus imp) {
    if (args.equals("set")) {
      doOptions();
      return DONE;
    }
    if (args.equals("about")) {
      showAbout();
      return DONE;
    }
    if (IJ.versionLessThan("1.27e"))
      return DONE;
    if (imp != null) {
      this.imp = imp;
    }
    return DOES_8G + DOES_16 + DOES_32;
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

  public void run(ImageProcessor ip) {
//    checkExistingRoi();
    (new Thread() {
      public void run() {
        double radius = MaudPreferences.getDouble("camera.defaultRadius", 194.71964);

	      Roi usableRoi = imp.getRoi();
        LaueOvalRoi roi = new LaueOvalRoi(imp, radius);
//        roi.usableRoi = usableArea;
        imp.setRoi(roi);
        if (getParameters((LaueOvalRoi) imp.getRoi()))
//		OvalSpectraPlot p = new OvalSpectraPlot(imp, radius);
          new SpectraProfilePlot(imp, false).createWindow(imp, usableRoi);
      }
    }).start();
  }

/*  public void checkExistingRoi() {
	  usableArea = imp.getRoi();
  }*/

  boolean getParameters(LaueOvalRoi roi) {

    RoiChangeDialog param = new RoiChangeDialog("Choose the integration lines",
            IJ.getInstance(), roi);

    param.addNumericField("Camera radius ("+imp.getCalibration().getUnit()+")", roi.getRadius(), 12);
    param.addNumericField("Center X ("+imp.getCalibration().getUnit()+")", roi.getX(), 6);
    param.addNumericField("Center Y ("+imp.getCalibration().getUnit()+")", roi.getY(), 6);
    param.addNumericField("Tracker radius (deg 2Theta)", roi.getCircle(), 2);
/*    param.addNumericField("Selection height ("+imp.getCalibration().getUnit()+")", MaudPreferences.getDouble(
            "camera.defaultHeight", 100.0), 2);*/

    param.addNumericField("Omega angle (in deg)", MaudPreferences.getDouble(
            "camera.defaultOmegaAngle", 15.0), 2);
    param.addNumericField("Chi angle (in deg)", MaudPreferences.getDouble(
            "camera.defaultChiAngle", 0.0), 2);
    param.addNumericField("Phi angle (in deg)", MaudPreferences.getDouble(
            "camera.defaultPhiAngle", 0.0), 2);
	  param.addNumericField("Points per pixels (for step generation)", MaudPreferences.getDouble(
			  "camera.pointsPerPixels", roi.getPointsPerPixels()), 2);
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
      double omega = param.getNextNumber();
      double chi = param.getNextNumber();
      double phi = param.getNextNumber();
      boolean calibrated = param.getNextBoolean();
	    double pointPerPixels = param.getNextNumber();
      MaudPreferences.setPref("camera.startingPointX", x);
      MaudPreferences.setPref("camera.startingPointY", y);
      MaudPreferences.setPref("camera.defaultOmegaAngle", omega);
      MaudPreferences.setPref("camera.defaultChiAngle", chi);
      MaudPreferences.setPref("camera.defaultPhiAngle", phi);
      MaudPreferences.setPref("anglesCalibration.imageToSpectra", calibrated);
	    MaudPreferences.setPref("camera.pointsPerPixels", pointPerPixels);
      roi.setRadius(radius);
      roi.setStartingPoint(x, y);
      roi.setCircle(diameter);
      roi.setOmega(omega);
      roi.setChi(chi);
      roi.setPhi(phi);
	    roi.setPointsPerPixels(pointPerPixels);
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
            " a spectrum from a curved 2D detector like an Image Plate\n" +
            " in a Debye-Scherrer type camera (need radius of the camera).");
  }

class RoiChangeDialog extends MaudGenericDialog {

  LaueOvalRoi roi = null;
  Button apply = null;

  public RoiChangeDialog(String title, Frame parent, LaueOvalRoi roi) {
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
      double omega = getNextNumber();
      double chi = getNextNumber();
      double phi = getNextNumber();
      boolean calibrated = getNextBoolean();
      MaudPreferences.setPref("camera.startingPointX", x);
      MaudPreferences.setPref("camera.startingPointY", y);
//        MaudPreferences.setPref("camera.defaultHeight", cameraHeight);
      MaudPreferences.setPref("camera.defaultOmegaAngle", omega);
      MaudPreferences.setPref("camera.defaultChiAngle", chi);
      MaudPreferences.setPref("camera.defaultPhiAngle", phi);
      MaudPreferences.setPref("anglesCalibration.imageToSpectra", calibrated);
      roi.setRadius(radius);
      roi.setStartingPoint(x, y);
      roi.setCircle(diameter);
//        roi.setSelHeight(cameraHeight);
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

/*class OvalSpectraPlot extends ProfilePlot {
	int npoints = 1000;
  double radius = 195.0;

	public OvalSpectraPlot(ImagePlus imp, double radius) {
		super();
		this.radius = radius;
		this.imp = imp;
		Roi roi = imp.getRoi();
		if (roi==null) {
			IJ.error("LaueOvalRoi selection not initialized.");
			return;
		}
		// RoiType is LaueOvalRoi
		imp.setColor(Color.white);
		Calibration cal = imp.getCalibration();
		pixelSize = cal.pixelWidth;
		units = cal.getUnits();
		yLabel = cal.getValueUnit();
		ImageProcessor ip = imp.getProcessor();
		ip.setCalibrationTable(cal.getCTable());
		profile = getOvalProfile(roi, ip);

		ip.setCalibrationTable(null);
		ImageWindow win = imp.getWindow();
		if (win!=null)
			magnification = win.getCanvas().getMagnification();
		else
			magnification = 1.0;
	}

	double[] getOvalProfile(Roi roi, ImageProcessor ip) {
		Rectangle b = roi.getBounds();
		double width = b.width;
		double height = b.height;
		// get radii from oval center
		double cx = b.x+width/2.0;
		double cy = b.y+height/2.0;
		// double theta0 = -Math.PI/2;
		double theta0 = 0;
		double tink = 2*Math.PI / npoints;
		double[] profile = new double[npoints];

		int i = 0;
		for(double theta = theta0; (theta < theta0+2*Math.PI) && (i<npoints); theta += tink) {
			double dx = Math.cos(theta);
			double dy = Math.sin(theta);
			double x = cx;
			double y = cy;

			double hotval = Double.MIN_VALUE;
			double hotx=0, hoty=0;
			while(roi.contains((int)x, (int)y)) {
				double val = ip.getInterpolatedPixel(x, y);
				if(val > hotval) {
					hotval = val;
					hotx = x;
					hoty = y;
				}
				x += dx;
				y += dy;
			}
			profile[i] = hotval;
			i++;
		}

		return profile;
	}

}*/


