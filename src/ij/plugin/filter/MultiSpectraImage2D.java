/*
 * @(#)MultiSpectraImage2D.java created Aug 8, 2003 Berkeley
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

package ij.plugin.filter;

import ij.AreaImage;
import ij.gui.*;
import ij.IJ;
import ij.process.ImageProcessor;
import it.unitn.ing.rista.diffr.cal.AngularFlatImageTransmissionCalibration;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.MaudPreferences;

import java.awt.*;
import java.awt.event.ActionEvent;


/**
 * The MultiSpectraImage2D is a class
 *
 * @version $Revision: 1.6 $, $Date: 2005/09/07 17:14:05 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MultiSpectraImage2D extends MultiSpectraGeneration {

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
        double radius = MaudPreferences.getDouble("image2D.DetectorDistance", 2000.0);
        LaueCircleStepRoi roi = new LaueCircleStepRoi(imp, radius);
//        roi.setUsableRectangle(usableRectangle);
        imp.setRoi(roi);
        if (getParameters((LaueCircleStepRoi) imp.getRoi()))
//		OvalSpectraPlot p = new OvalSpectraPlot(imp, radius);
          new SpectraProfilePlot(imp, false).createWindow(imp);
      }
    }).start();
  }

  boolean getParameters(LaueCircleStepRoi roi) {
    MultiRoiChangeDialog param = new MultiRoiChangeDialog("Choose the integration lines",
            IJ.getInstance(), roi);

	  roi.setRadius(AreaImage.getData().getInstrument().getAngularCalibration().getRadius());
	  roi.setCenterX(AreaImage.getData().getInstrument().getAngularCalibration().getOriginalCenterX());
	  roi.setCenterY(AreaImage.getData().getInstrument().getAngularCalibration().getOriginalCenterY());

    param.addNumericField("Sample-Detector distance ("+imp.getCalibration().getUnit()+")", roi.getRadius(), 6);
    param.addNumericField("Center X ("+imp.getCalibration().getUnit()+")", roi.getX(), 6);
    param.addNumericField("Center Y ("+imp.getCalibration().getUnit()+")", roi.getY(), 6);
    param.addNumericField("Tracker circle radius (deg 2Theta)", roi.getCircle(), 2);
//    param.addNumericField("Selection height (mm)", MaudPreferences.getDouble(
//            "camera.defaultHeight", 100.0), 2);
    param.addNumericField("Starting angle (in deg)", MaudPreferences.getDouble(
            "image2D.StartingAngle", 0.0), 2);
    param.addNumericField("Final angle (in deg)", MaudPreferences.getDouble(
            "image2D.FinalAngle", 360.0), 2);
    param.addNumericField("Number of spectra", MaudPreferences.getInteger(
            "image2D.nSpectraDivision", 72), 0);
    param.addNumericField("Omega angle (in deg)", MaudPreferences.getDouble(
            "camera.defaultOmegaAngle", 15.0), 2);
    param.addNumericField("Chi angle (in deg)", MaudPreferences.getDouble(
            "camera.defaultChiAngle", 0.0), 2);
    param.addNumericField("Phi angle (in deg)", MaudPreferences.getDouble(
            "camera.defaultPhiAngle", 0.0), 2);
    param.addCheckbox("2-Theta angles calibrated",
            MaudPreferences.getBoolean("anglesCalibration.imageToSpectra", false));
    param.addNumericField("Image tilting Sx", MaudPreferences.getDouble(
            "image2D.tiltingErrorSx", 0.0), 6);
    param.addNumericField("Image tilting Sy", MaudPreferences.getDouble(
            "image2D.tiltingErrorSy", 0.0), 6);
    param.addCheckbox("Reflection image", MaudPreferences.getBoolean(
            "image2D.reflection", false));
//		param.addChoice("Analysis mode", modes, modes[mode]);
//		param.addCheckbox("Show Hotspots", hotspots);

    param.showDialog();
    if (!param.wasCanceled()) {
//      radius = param.getNextNumber();
//			mode = param.getNextChoiceIndex();
//			hotspots = param.getNextBoolean();
      param.resetIndices();
      double radius = param.getNextNumber();
      MaudPreferences.setPref("image2D.DetectorDistance", radius);
	    AreaImage.getData().getInstrument().
			    getAngularCalibration().setRadius(Double.toString(radius));
      double x = param.getNextNumber();
      double y = param.getNextNumber();
      double diameter = param.getNextNumber();
      double startingAngle = param.getNextNumber();
      double finalAngle = param.getNextNumber();
      int nDivision = (int) param.getNextNumber();
      double omega = param.getNextNumber();
      double chi = param.getNextNumber();
      double phi = param.getNextNumber();
      boolean calibrated = param.getNextBoolean();
      double Sx = param.getNextNumber();
      double Sy = param.getNextNumber();
      boolean reflection = param.getNextBoolean();
      MaudPreferences.setPref("image2D.centerX", x);
      MaudPreferences.setPref("image2D.centerY", y);
	    AreaImage.getData().getInstrument().
			    getAngularCalibration().setOriginalCenterX(Double.toString(x));
	    AreaImage.getData().getInstrument().
			    getAngularCalibration().setOriginalCenterY(Double.toString(y));
      MaudPreferences.setPref("image2D.roiCircle", diameter);
      MaudPreferences.setPref("image2D.StartingAngle", startingAngle);
      MaudPreferences.setPref("image2D.FinalAngle", finalAngle);
      MaudPreferences.setPref("image2D.nSpectraDivision", nDivision);
      MaudPreferences.setPref("camera.defaultOmegaAngle", omega);
      MaudPreferences.setPref("camera.defaultChiAngle", chi);
      MaudPreferences.setPref("camera.defaultPhiAngle", phi);
      MaudPreferences.setPref("anglesCalibration.imageToSpectra", calibrated);
      MaudPreferences.setPref("image2D.tiltingErrorSx", Sx);
      MaudPreferences.setPref("image2D.tiltingErrorSy", Sy);
      MaudPreferences.setPref("image2D.reflection", reflection);
      roi.setRadius(radius);
      roi.setStartingPoint(x, y);
      roi.setCircle(diameter);
      roi.startingAngle = startingAngle;
      roi.finalAngle = finalAngle;
      roi.nprofiles = nDivision;
      roi.setOmega(omega);
      roi.setChi(chi);
      roi.setPhi(phi);
      roi.Sx = Sx;
      roi.Sy = Sy;
      if (calibrated)
        roi.setCalibrated();
      else
        roi.setUncalibrated();
      roi.setInReflection(true);
      roi.updateSelection();
      return true;
    } else
      return false;
  }

  void showAbout() {
    IJ.showMessage("About OvalSpectraSelection", " Transform Laue circles to\n" +
            " spectrua from a curved 2D detector like an Image Plate\n" +
            " in a Debye-Scherrer type camera (need radius of the camera)." +
            " Spectra are generated for different cone intervals");
  }

  class MultiRoiChangeDialog extends MaudGenericDialog {

    LaueCircleStepRoi roi = null;
    Button apply = null;

    public MultiRoiChangeDialog(String title, Frame parent, LaueCircleStepRoi roi) {
      super(title, parent);
      this.roi = roi;
    }

    /** Displays this dialog box. */
    public void showDialog() {
      apply = new Button("Update");
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
        double startingAngle = getNextNumber();
        double finalAngle = getNextNumber();
        int nDivision = (int) getNextNumber();
        double omega = getNextNumber();
        double chi = getNextNumber();
        double phi = getNextNumber();
        boolean calibrated = getNextBoolean();
        double Sx = getNextNumber();
        double Sy = getNextNumber();
        MaudPreferences.setPref("image2D.centerX", x);
        MaudPreferences.setPref("image2D.centerY", y);
        MaudPreferences.setPref("image2D.roiCircle", diameter);
        MaudPreferences.setPref("image2D.StartingAngle", startingAngle);
        MaudPreferences.setPref("image2D.FinalAngle", finalAngle);
        MaudPreferences.setPref("image2D.nSpectraDivision", nDivision);
        MaudPreferences.setPref("camera.defaultOmegaAngle", omega);
        MaudPreferences.setPref("camera.defaultChiAngle", chi);
        MaudPreferences.setPref("camera.defaultPhiAngle", phi);
        MaudPreferences.setPref("anglesCalibration.imageToSpectra", calibrated);
        MaudPreferences.setPref("image2D.tiltingErrorSx", Sx);
        MaudPreferences.setPref("image2D.tiltingErrorSy", Sy);
        roi.setRadius(radius);
        roi.setStartingPoint(x, y);
        roi.setCircle(diameter);
        roi.startingAngle = startingAngle;
        roi.finalAngle = finalAngle;
        roi.nprofiles = nDivision;
        roi.setOmega(omega);
        roi.setChi(chi);
        roi.setPhi(phi);
        roi.Sx = Sx;
        roi.Sy = Sy;
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

