/*
 * @(#)MultiSpectraFromReflectionFlatCCD.java created Jun 5, 2007 Caen
 *
 * Copyright (c) 2007 Luca Lutterotti All Rights Reserved.
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

import ij.IJ;
import ij.AreaImage;
import ij.gui.*;
import ij.plugin.KCDReader;
import ij.plugin.BrukerImageReader;
import ij.process.ImageProcessor;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.cal.AngularInclinedFlatImageCalibration;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.Properties;

/**
 * The MultiSpectraFromReflectionFlatCCD is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 5, 2007 3:27:30 PM $
 * @since JDK1.1
 */
public class MultiSpectraFromReflectionFlatCCD extends OvalSpectraSelection {

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
        double Dd = AreaImage.getData().getInstrument().getAngularCalibration().getRadius();
        FlatCCDReflectionSquareRoi roi = new FlatCCDReflectionSquareRoi(imp, Dd);
	      Roi usableRoi = imp.getRoi();
        imp.setRoi(roi);
        if (getParameters((FlatCCDReflectionSquareRoi) imp.getRoi()))
//		OvalSpectraPlot p = new OvalSpectraPlot(imp, radius);
          new SpectraProfilePlot(imp, false).createWindow(imp, usableRoi);
      }
    }).start();
  }

  boolean getParameters(FlatCCDReflectionSquareRoi roi) {
    MultiRoiChangeDialog param = new MultiRoiChangeDialog("Choose the integration lines",
            IJ.getInstance(), roi);

    Properties prop = imp.getProperties();

    double omega = MaudPreferences.getDouble("sample.defaultOmegaAngle", 15.0);
    double chi = MaudPreferences.getDouble("sample.defaultChiAngle", 0.0);
    double phi = MaudPreferences.getDouble("sample.defaultPhiAngle", 0.0);
    double theta2 = ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
        getAngularCalibration()).getOriginal2Theta();
    double azimuthal = ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
        getAngularCalibration()).getOriginalPhiDA();
    double phiDetector = ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
        getAngularCalibration()).getOriginalOmegaDN();
    double coneAngle = ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
        getAngularCalibration()).getOriginalEtaDA();

    double centerX = (AreaImage.getData().getInstrument().
        getAngularCalibration()).getOriginalCenterX();
    double centerY = (AreaImage.getData().getInstrument().
        getAngularCalibration()).getOriginalCenterY();

	  roi.setRadius(((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
			  getAngularCalibration()).getOriginalDistance());

    if (prop != null && prop.containsKey(KCDReader.kappa_support_angle)) {
      double alpha = getDouble(prop, KCDReader.kappa_support_angle, 50.0);
      double omegaKappa = getDouble(prop, KCDReader.omega_start, 0.0) +
          getDouble(prop, KCDReader.omega_scan_range, 0.0)
          / 2.0;
      double kappa = getDouble(prop, KCDReader.kappa_start, 0.0) +
          getDouble(prop, KCDReader.kappa_scan_range, 0.0)
          / 2.0;
      double phiKappa = getDouble(prop, KCDReader.phi_start, 0.0) +
          getDouble(prop, KCDReader.phi_scan_range, 0.0)
          / 2.0;
      double[] omegaChiPhi = Angles.eulerianFromKappa(alpha, omegaKappa, kappa, phiKappa);
      for (int i = 0; i < omegaChiPhi.length; i++)
      if (Math.abs(omegaChiPhi[i]) < 1.0E-5)
        omegaChiPhi[i] = 0.0;
      omega = omegaChiPhi[0];
      chi = omegaChiPhi[1];
      phi = omegaChiPhi[2];
      theta2 = getDouble(prop, KCDReader.theta_start, 0.0) * 2.0 +
          getDouble(prop, KCDReader.theta_scan_range, 0.0);
      double dx = getDouble(prop, KCDReader.dx_start, 0.0) +
          getDouble(prop, KCDReader.dx_scan_range, 0.0)
          / 2.0;
      roi.setRadius(dx);
      double xdim = getDouble(prop, KCDReader.x_dimension, 0.0);
      double ydim = getDouble(prop, KCDReader.y_dimension, 0.0);
      if (roi.getX() > xdim || roi.getY() > ydim)
        roi.setStartingPoint(xdim / 2, ydim / 2);
    }

    if (prop != null && prop.containsKey(BrukerImageReader.brukerImage)) {
      omega = getDouble(prop, BrukerImageReader.omegaString, omega);
      chi = getDouble(prop, BrukerImageReader.chiString, chi);
      phi = getDouble(prop, BrukerImageReader.phiString, phi);
      theta2 = getDouble(prop, BrukerImageReader.theta2String, theta2);
      double dx = getDouble(prop, BrukerImageReader.detector_distance, roi.getRadius());
      roi.setRadius(dx);
      centerX = getDouble(prop, BrukerImageReader.center_x, centerX);
      centerY = getDouble(prop, BrukerImageReader.center_y, centerY);
    }

    param.addNumericField("Detector distance ("+imp.getCalibration().getUnit()+")", roi.getRadius(), 6);
    param.addNumericField("Center X ("+imp.getCalibration().getUnit()+")", centerX, 6);
    param.addNumericField("Center Y ("+imp.getCalibration().getUnit()+")", centerY, 6);
    param.addNumericField("2Theta test circle (deg)",
        MaudPreferences.getDouble("pixelDetector.trackerRadius", roi.getCircle()), 2);
/*    param.addNumericField("Selection height ("+imp.getCalibration().getUnit()+")", MaudPreferences.getDouble(
            "camera.defaultHeight", 100.0), 2);*/
    if (prop != null && prop.containsKey(BrukerImageReader.brukerImage)) {
    param.addNumericField("Eta step for spectra (deg)", MaudPreferences.getDouble(
            "brukerImage.defaultDiffractionConeInterval", 5.0), 2);
    param.addNumericField("2theta step for spectra (deg)", MaudPreferences.getDouble(
            "brukerImage.defaultDiffractionStepAngle", 0.02), 4);
    } else {
      param.addNumericField("Eta step for spectra (deg)", MaudPreferences.getDouble(
              "pixelDetector.defaultDiffractionConeInterval", 5.0), 2);
      param.addNumericField("2theta step for spectra (deg)", MaudPreferences.getDouble(
              "pixelDetector.defaultDiffractionStepAngle", 0.02), 4);
    }
    param.addNumericField("Sample omega angle (deg)", omega, 2);
    param.addNumericField("Sample chi angle (deg)", chi, 2);
    param.addNumericField("Sample phi angle (deg)", phi, 2);
    param.addNumericField("Detector 2theta angle (deg)", theta2, 6);
    param.addNumericField("Detector azimuthal angle (deg)", azimuthal, 6);
    param.addNumericField("Detector eta angle (deg)", coneAngle, 6);
    param.addNumericField("Detector in plane phi rotation (deg)", phiDetector, 6);
//		param.addChoice("Analysis mode", modes, modes[mode]);
//		param.addCheckbox("Show Hotspots", hotspots);

    param.showDialog();
    if (!param.wasCanceled()) {
//      radius = param.getNextNumber();
//			mode = param.getNextChoiceIndex();
//			hotspots = param.getNextBoolean();
      param.resetIndices();
      double radius = param.getNextNumber();
      AreaImage.getData().getInstrument().
        getAngularCalibration().setRadius(Double.toString(radius));
      double x = param.getNextNumber();
      double y = param.getNextNumber();
      double diameter = param.getNextNumber();
//      double cameraHeight = param.getNextNumber();
      double coneAngleStep = param.getNextNumber();
      double theta2AngleStep = param.getNextNumber();
      omega = param.getNextNumber();
      chi = param.getNextNumber();
      phi = param.getNextNumber();
      double sigmaDA = param.getNextNumber();
      double phiDA = param.getNextNumber();
      double etaDA = param.getNextNumber();
      double omegaDN = param.getNextNumber();
      ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
        getAngularCalibration()).setOriginalCenterX(x);
      ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
        getAngularCalibration()).setOriginalCenterY(y);
	    ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
			    getAngularCalibration()).setOriginalDistance(radius);
	    ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
			    getAngularCalibration()).setOriginal2Theta(sigmaDA);
	    ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
			    getAngularCalibration()).setOriginalPhiDA(phiDA);
	    ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
			    getAngularCalibration()).setOriginalOmegaDN(omegaDN);
	    ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
			    getAngularCalibration()).setOriginalEtaDA(etaDA);
      MaudPreferences.setPref("pixelDetector.trackerRadius", diameter);
//      MaudPreferences.setPref("camera.defaultHeight", cameraHeight);
      if (prop != null && prop.containsKey(BrukerImageReader.brukerImage)) {
        MaudPreferences.setPref("brukerImage.defaultDiffractionConeInterval", coneAngleStep);
        MaudPreferences.setPref("brukerImage.defaultDiffractionStepAngle", theta2AngleStep);
      } else {
        MaudPreferences.setPref("pixelDetector.defaultDiffractionConeInterval", coneAngleStep);
        MaudPreferences.setPref("pixelDetector.defaultDiffractionStepAngle", theta2AngleStep);
      }
      MaudPreferences.setPref("sample.defaultOmegaAngle", omega);
      MaudPreferences.setPref("sample.defaultChiAngle", chi);
      MaudPreferences.setPref("sample.defaultPhiAngle", phi);
	    ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
			    getAngularCalibration()).setDetectorDistance(radius);
	    ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
			    getAngularCalibration()).setDetectorCenterX(0);
	    ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
			    getAngularCalibration()).setDetectorCenterY(0);
      ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
        getAngularCalibration()).setDetector2Theta(sigmaDA);
      ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
        getAngularCalibration()).setDetectorPhiDA(phiDA);
      ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
        getAngularCalibration()).setDetectorOmegaDN(omegaDN);
      ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
        getAngularCalibration()).setDetectorEtaDA(etaDA);
	    MaudPreferences.setPref("pixelDetector.defaultDetectorDistance", radius);
	    MaudPreferences.setPref("pixelDetector.default2ThetaAngle", sigmaDA);
	    MaudPreferences.setPref("pixelDetector.defaultPhiDAangle", phiDA);
	    MaudPreferences.setPref("pixelDetector.defaultOmegaDNangle", omegaDN);
	    MaudPreferences.setPref("pixelDetector.defaultEtaDAangle", etaDA);
      roi.setRadius(radius);
      roi.setStartingPoint(x, y);
      roi.setCircle(diameter);
//      roi.setSelHeight(cameraHeight);
      roi.setConeInterval(coneAngleStep);
      roi.set2thetaStep(theta2AngleStep);
      roi.setOmega(omega);
      roi.setChi(chi);
      roi.setPhi(phi);
      roi.setOmegaDN(omegaDN);
      roi.setDet2Theta(sigmaDA);
      roi.setPhiDA(phiDA);
      roi.setEtaDA(etaDA);
      roi.setCalibrated();
      roi.updateSelection();
      return true;
    } else
      return false;
  }

  public static double getDouble(Properties props, String key, double defaultValue) {
    Double n = getNumber(props, key);
    return n != null ? n.doubleValue() : defaultValue;
  }

  public static Double getNumber(Properties props, String key) {
    String s = (String) props.get(key);
//    System.out.println("Retrieving " + key + " = " + s);
    if (s != null) {
      try {
        return Double.parseDouble(s);
      } catch (NumberFormatException e) {
	      e.printStackTrace();
        // LogSystem.printStackTrace(e);
      }
    }
    return null;
  }

  void showAbout() {
    IJ.showMessage("About MultiSpectraFromReflectionFlatCCD", " Transform image to\n" +
            " spectra from flat 2D detector in reflection like an Image Plate or CCD.\n" +
            " Spectra are generated for different cone intervals");
  }

  class MultiRoiChangeDialog extends MaudGenericDialog {

    FlatCCDReflectionSquareRoi roi = null;
    Button apply = null;

    public MultiRoiChangeDialog(String title, Frame parent, FlatCCDReflectionSquareRoi roi) {
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
        MaudPreferences.setPref("pixelDetector.defaultDistance", radius);
        double x = getNextNumber();
        double y = getNextNumber();
        double diameter = getNextNumber();
//      double cameraHeight = param.getNextNumber();
        double coneAngleStep = getNextNumber();
        double theta2AngleStep = getNextNumber();
        double omega = getNextNumber();
        double chi = getNextNumber();
        double phi = getNextNumber();
        double sigmaDA = getNextNumber();
        double phiDA = getNextNumber();
        double etaDA = getNextNumber();
        double omegaDN = getNextNumber();
        ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
          getAngularCalibration()).setOriginalCenterX(x);
        ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
          getAngularCalibration()).setOriginalCenterY(y);
        MaudPreferences.setPref("pixelDetector.trackerRadius", diameter);
//      MaudPreferences.setPref("camera.defaultHeight", cameraHeight);
        Properties prop = imp.getProperties();
        if (prop != null && prop.containsKey(BrukerImageReader.brukerImage)) {
        MaudPreferences.setPref("brukerImage.defaultDiffractionConeInterval", coneAngleStep);
        MaudPreferences.setPref("brukerImage.defaultDiffractionStepAngle", theta2AngleStep);
        } else {
        MaudPreferences.setPref("pixelDetector.defaultDiffractionConeInterval", coneAngleStep);
        MaudPreferences.setPref("pixelDetector.defaultDiffractionStepAngle", theta2AngleStep);
        }
        MaudPreferences.setPref("sample.defaultOmegaAngle", omega);
        MaudPreferences.setPref("sample.defaultChiAngle", chi);
        MaudPreferences.setPref("sample.defaultPhiAngle", phi);
        ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
          getAngularCalibration()).setDetector2Theta(sigmaDA);
        ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
          getAngularCalibration()).setDetectorPhiDA(phiDA);
        ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
          getAngularCalibration()).setDetectorOmegaDN(omegaDN);
        ((AngularInclinedFlatImageCalibration) AreaImage.getData().getInstrument().
          getAngularCalibration()).setDetectorEtaDA(etaDA);
        roi.setRadius(radius);
        roi.setStartingPoint(x, y);
        roi.setCircle(diameter);
//      roi.setSelHeight(cameraHeight);
        roi.setConeInterval(coneAngleStep);
        roi.set2thetaStep(theta2AngleStep);
        roi.setOmega(omega);
        roi.setChi(chi);
        roi.setPhi(phi);
        roi.setOmegaDN(omegaDN);
        roi.setDet2Theta(sigmaDA);
        roi.setPhiDA(phiDA);
        roi.setEtaDA(etaDA);
        roi.setCalibrated();
        roi.updateSelection();
      } else
        super.actionPerformed(e);
    }

  }

}
