package ij.gui;

import java.awt.*;

import ij.*;
import ij.measure.*;
import ij.process.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.Utility;

import java.io.*;

// Oval region of interest

public class LaueOvalStepRoi extends OpenRoi {

  double radius = 194.71964;
  double pixelSizeX = 1.0;
  double pixelSizeY = 1.0;
  double centerX = 0.0;
  double centerY = 0.0;
  double diameter = 0.0;
//  double selHeight = 0.0;
  double coordTrasfX; // = pixelSize * MoreMath.InchesTomm;
  double coordTrasfY; // = pixelSize * MoreMath.InchesTomm;

  double coneInterval = 5.0;
  double coneAngleMax = 5.0;
  double omega = 15.0;
  double chi = 0.0;
  double phi = 0.0;
  boolean calibrated = true;
	double pointsPerPixels = 1;

  public LaueOvalStepRoi(ImagePlus imp, double radius) {
    this(MaudPreferences.getDouble("camera.startingPointX", 0.0),
            MaudPreferences.getDouble("camera.startingPointY", 64.0),
            imp.getWidth() * imp.getCalibration().pixelWidth / 2.0,
            imp);
    setRadius(radius);
  }

  // Creates a new OvalRoi. The ImagePlus argument can be null.
  public LaueOvalStepRoi(double x, double y, double diameter, ImagePlus imp) {
    super(0, 0, imp.getWidth(), imp.getHeight(), imp);
    type = OVAL;
    ij.measure.Calibration cal = imp.getCalibration();
    pixelSizeX = cal.pixelWidth;
    pixelSizeY = cal.pixelHeight;
    coordTrasfX = pixelSizeX;// * MoreMath.InchesTomm;
    coordTrasfY = pixelSizeY;// * MoreMath.InchesTomm;
    this.centerX = x;
    this.centerY = y;
    this.diameter = diameter;
  }

  // Starts the process of creating a user-defined OvalRoi.
  public LaueOvalStepRoi(int x, int y, ImagePlus imp) {
    super(x, y, imp);
    type = OVAL;
  }

  public void setRadius(double radius) {
    this.radius = radius;
  }

  public double getRadius() {
    return radius;
  }

  public double getCircle() {
    return diameter;
  }

  public double getX() {
    return centerX;
  }

  public double getY() {
    return centerY;
  }

  public void setCircle(double diameter) {
    this.diameter = diameter;
  }

	public void setPointsPerPixels(double value) {
//    tmat = null;
		pointsPerPixels = Math.abs(value);
	}

	public double getPointsPerPixels() {
		return pointsPerPixels;
	}

  public void setStartingPoint(double xNew, double yNew) {
    centerX = xNew;
    centerY = yNew;
    startX = (int) xNew;
    startY = (int) yNew;
  }

  public void setConeInterval(double coneInterval) {
    this.coneInterval = Math.abs(coneInterval);
  }

  public double getConeInterval() {
    return coneInterval;
  }

  public void setConeAngleMax(double coneAngleMax) {
    this.coneAngleMax = Math.abs(coneAngleMax);

  }

  public double getConeAngleMax() {
    return coneAngleMax;
  }

  public void setOmega(double omega) {
    this.omega = Math.abs(omega);
  }

  public double getOmega() {
    return omega;
  }

  public void setChi(double chi) {
    this.chi = Math.abs(chi);
  }

  public double getChi() {
    return chi;
  }

  public void setPhi(double phi) {
    this.phi = Math.abs(phi);
  }

  public double getPhi() {
    return phi;
  }

  public void setCalibrated() {
    calibrated = true;
  }

  public void setUncalibrated() {
    calibrated = false;
  }

  public void updateSelection() {
    updateClipRect();
//    imp.draw(clipX, clipY, clipWidth, clipHeight);
    imp.updateAndRepaintWindow();
    oldX = x;
    oldY = y;
    oldWidth = width;
    oldHeight = height;
  }

  protected void moveHandle(int ox, int oy) {
    if (clipboard!=null)
      return;
    //IJ.log("moveHandle: "+activeHandle+" "+ox+" "+oy);
    int x1=x, y1=y, x2=x1+width, y2=y+height;
    int w2 = (int)(0.14645*width);
    int h2 = (int)(0.14645*height);
    switch (activeHandle) {
      case 0: x=ox-w2; y=oy-h2; break;
      case 1: y=oy; break;
      case 2: x2=ox+w2; y=oy-h2; break;
      case 3: x2=ox; break;
      case 4: x2=ox+w2; y2=oy+h2; break;
      case 5: y2=oy; break;
      case 6: x=ox-w2; y2=oy+h2; break;
      case 7: x=ox; break;
    }
    if (x<0) x=0; if (y<0) y=0;
    if (x<x2)
       width=x2-x;
    else
      {width=1; x=x2;}
    if (y<y2)
       height = y2-y;
    else
       {height=1; y=y2;}
    if (constrain)
      height = width;
    if ((x+width)>xMax) width=xMax-x;
    if ((y+height)>yMax) height=yMax-y;
    updateClipRect();
    imp.draw(clipX, clipY, clipWidth, clipHeight);
    oldX=x; oldY=y;
    oldWidth=width; oldHeight=height;
    cachedMask = null;
  }

  public void draw(Graphics g) {
    int y1 = 0;
    int yMax = this.yMax;
/*    if (usableRectangle != null) {
      y1 = (int) usableRectangle.getMinY();
      yMax = (int) usableRectangle.getMaxY();
      int x1 = (int) usableRectangle.getMinX();
      int xMax = (int) usableRectangle.getMaxX();
      g.setColor(Color.GREEN);
      g.drawRect(ic.screenX(x1), ic.screenY(y1), ic.screenX(xMax-x1), ic.screenY(yMax-y1));
    }*/
    g.setColor(Color.RED);
    mag = ic!=null?ic.getMagnification():1.0;
	  double[] x1 = getXCoordSym(y1);
    int y2 = y1;
    while (y2 < yMax) {
      y2++;
      double[] x2 = getXCoordSym(y2);
      for (int i = 0; i < 2; i++) {
        if (!Double.isNaN(x1[i]) && !Double.isNaN(x2[i]) && contains((int) x2[i], y2) && contains((int) x1[i], y1)) {
          g.drawLine(ic.screenX((int) x1[i]), ic.screenY(y1), ic.screenX((int) x2[i]), ic.screenY(y2));
//          System.out.println("Draw line from " + x1[i] + ", " + y1 + " to " + x2[i] + ", " + y2);
        }
        x1[i] = x2[i];
      }
      y1 = y2;
    }
    if (updateFullWindow) {
      updateFullWindow = false;
      imp.draw();
    }
    if (state!=NORMAL)
      showStatus();
  }

  public double getXCoord(double x, double ycoord) {
	  double arg;
	  double tan2theta = MoreMath.tand(x);
	  tan2theta *= tan2theta;
	  tan2theta += 1;
    double t = (ycoord * coordTrasfY - getY()) / radius;
	  if (x > 90)
	    arg = Math.acos(-Math.sqrt((t * t + 1) / tan2theta));
	  else
	    arg = Math.acos(Math.sqrt((t * t + 1) / tan2theta));
//	  System.out.println(arg);
    return (radius * arg + getX()) / coordTrasfX;
  }

  public double[] getXCoordSym(double ycoord) {
	  double x = getXCoord(getCircle(), ycoord);
    double[] coordX = new double[2];
	  coordX[0] = x;
	  coordX[1] = -x + getX() / coordTrasfX;
    return coordX;
  }

  public double getYCoord(double ycoord) {
    return ycoord * coordTrasfY - getY();
  }

  /** Draws an outline of this OvalRoi on the image. */
  public void drawPixels(ImageProcessor ip) {
    Polygon p = getPolygon();
    ip.drawPolygon(p);
    if (Line.getWidth()>1)
      updateFullWindow = true;
  }

  /** Returns this OvalRoi as a polygon. */
  public Polygon getPolygon() {
    ImageProcessor mask = getMask();
    Wand wand = new Wand(mask);
    wand.autoOutline(width/2,height/2, 255, 255);
        for (int i=0; i<wand.npoints; i++) {
            wand.xpoints[i] += x;
            wand.ypoints[i] += y;
        }
    return new Polygon(wand.xpoints, wand.ypoints, wand.npoints);
  }

  /** Returns a handle number if the specified screen coordinates are
    inside or near a handle, otherwise returns -1. */
  public int isHandle(int sx, int sy) {
    if (clipboard!=null || ic==null) return -1;
    double mag = ic.getMagnification();
    int size = HANDLE_SIZE+3;
    int halfSize = size/2;
    int sx1 = ic.screenX(x) - halfSize;
    int sy1 = ic.screenY(y) - halfSize;
    int sx3 = ic.screenX(x+width) - halfSize;
    int sy3 = ic.screenY(y+height) - halfSize;
    int sx2 = sx1 + (sx3 - sx1)/2;
    int sy2 = sy1 + (sy3 - sy1)/2;

    int sw2 = (int)(0.14645*(sx3-sx1));
    int sh2 = (int)(0.14645*(sy3-sy1));

    if (sx>=sx1+sw2&&sx<=sx1+sw2+size&&sy>=sy1+sh2&&sy<=sy1+sh2+size) return 0;
    if (sx>=sx2&&sx<=sx2+size&&sy>=sy1&&sy<=sy1+size) return 1;
    if (sx>=sx3-sw2&&sx<=sx3-sw2+size&&sy>=sy1+sh2&&sy<=sy1+sh2+size) return 2;
    if (sx>=sx3&&sx<=sx3+size&&sy>=sy2&&sy<=sy2+size) return 3;
    if (sx>=sx3-sw2&&sx<=sx3-sw2+size&&sy>=sy3-sh2&&sy<=sy3-sh2+size) return 4;
    if (sx>=sx2&&sx<=sx2+size&&sy>=sy3&&sy<=sy3+size) return 5;
    if (sx>=sx1+sw2&&sx<=sx1+sw2+size&&sy>=sy3-sh2&&sy<=sy3-sh2+size) return 6;
    if (sx>=sx1&&sx<=sx1+size&&sy>=sy2&&sy<=sy2+size) return 7;
    return -1;
  }

  public ImageProcessor getMask() {
    if (cachedMask!=null && cachedMask.getPixels()!=null)
      return cachedMask;
    ImageProcessor mask = new ByteProcessor(width, height);
    double a=width/2.0, b=height/2.0;
    double a2=a*a, b2=b*b;
        a -= 0.5; b -= 0.5;
    double xx, yy;
        int offset;
        byte[] pixels = (byte[])mask.getPixels();
    for (int y=0; y<height; y++) {
            offset = y*width;
      for (int x=0; x<width; x++) {
        xx = x - a;
        yy = y - b;
        if ((xx*xx/a2+yy*yy/b2)<=1.0)
          pixels[offset+x] = -1;
      }
    }
    cachedMask = mask;
    return mask;
  }

  public double[][] getIntervalPixels() {
    ImageProcessor ip = imp.getProcessor();
    if (ip == null) {
      System.out.println("No image processor!");
      return null;
    }

//	  Roi selroi = usableRoi;
//    int maxGray = 256;

    int etaIndex = 0;
    double coneStep = getConeInterval();
    double halfConeStep = coneStep / 2.0;
    int nprofiles = (int) (2.0 * getConeAngleMax() / getConeInterval() + 1.000001);
    int centerIndex = nprofiles / 2;
    int[] numbAvg = new int[nprofiles];

    int counter = 0;

	  double correctionExponent = MaudPreferences.getDouble("image2D.exponentCorrectionValue", 0.0);

    int minX = MaudPreferences.getInteger("ovalROI.minX", 0);
    int maxX = MaudPreferences.getInteger("ovalROI.maxX", xMax);
    int minY = MaudPreferences.getInteger("ovalROI.minY", 0);
    int maxY = MaudPreferences.getInteger("ovalROI.maxY", yMax);

	  if (usableRoi != null) {
	  	Rectangle usableRectangle = usableRoi.getBounds();
		  if (usableRectangle != null) {
			  minX = (int) usableRectangle.getMinX();
			  maxX = (int) usableRectangle.getMaxX();
			  minY = (int) usableRectangle.getMinY();
			  maxY = (int) usableRectangle.getMaxY();
		  }
	  }

    MaudPreferences.setPref("ovalROI.minX", minX);
    MaudPreferences.setPref("ovalROI.maxX", maxX);
    MaudPreferences.setPref("ovalROI.minY", minY);
    MaudPreferences.setPref("ovalROI.maxY", maxY);

    int npoints = maxX - minX;
    double[][] profile = new double[nprofiles][npoints];
    for (int ix = minX; ix < maxX; ix++) {
      for (etaIndex = 0; etaIndex < nprofiles; etaIndex++) {
        profile[etaIndex][ix - minX] = 0.0;
        numbAvg[etaIndex] = 0;
      }
	    double xreal = (1.0 / getPointsPerPixels() * ix + minX) * coordTrasfX - getX();
	    double twoTheta = xreal / radius * Constants.PITODEG;
      for (int iy = minY; iy < maxY; iy++) {
	      double x1 = getXCoord(twoTheta, iy);
        double y1 = getYCoord(iy);
        double eta = 0.0;
        if (twoTheta != 0.0) {
          double arg = -y1 / (radius * MoreMath.sind(twoTheta));  // right handed rotation from the source
          eta = Constants.PITODEG * Math.atan(arg);
        }
        if (Math.abs(eta) < getConeAngleMax()) {
//          eta = getRoundedEta(eta);
          double coreta = Math.pow(1.0 + (y1 * y1 / radius / radius), correctionExponent); // 1.0 / MoreMath.cosd(eta);

          int sign = 1;
          if ((eta < 0.0 && twoTheta >= 90.0) || (eta > 0.0 && twoTheta < 90.0))
            sign = -1;
          etaIndex = centerIndex + sign * (int) ((Math.abs(eta) + halfConeStep) / coneStep);
          if ((usableRoi != null && usableRoi.contains((int) x1, iy)) ||
		          (usableRoi == null && (x1 >= minX && x1 < maxX))) {
            profile[etaIndex][ix - minX] += ip.getInterpolatedPixel(x1, (double) iy) * coreta;
//      System.out.println("x:  " + x1 + " " + y1 + " " + coreta + " " + profile[etaIndex][ix - minX]);
            numbAvg[etaIndex]++;
          }// else
// System.out.println("Not x:  " + x1 + " " + iy);
        }
      }
      for (etaIndex = 0; etaIndex < nprofiles; etaIndex++) {
        if (numbAvg[etaIndex] > 3) {
          profile[etaIndex][ix - minX] /= numbAvg[etaIndex];
//        if (profile[ix] > maxGray) maxGray *= 256;
        } else
          profile[etaIndex][ix - minX] = -1.0;
      }

    }

    saveAsText(profile, nprofiles, minX, maxX, coneStep, centerIndex);
//      System.out.println(profile[ix] + " " + maxGray);
    return profile;

  }

  static int LAUE_OVAL_STEP_ROI = -999; // to be unique

  public int getType() {
    return LaueOvalStepRoi.LAUE_OVAL_STEP_ROI;
  }

  void saveAsText(double[][] profile, int nprofiles, int startX, int endX, double coneStep, int centerIndex) {

    DataFileSet data = AreaImage.getData();
    String filename = Utility.openFileDialog(new Frame(), "Save as CIF (.esg)...",
            FileDialog.SAVE, data.getFilePar().getDirectory(), null, "put a name (with extension).esg");
    if (filename == null)
      return;

    String[] folderAndName = Misc.getFolderandName(filename);

    String folder = folderAndName[0];
    filename = folderAndName[1];

    if (filename == null) return;
    if (Constants.sandboxEnabled && !filename.endsWith(".esg"))
      filename = filename + ".esg";

    IJ.wait(250);  // give system time to redraw ImageJ window
    IJ.showStatus("Saving plot values...");
    String title = "noTitle";
    BufferedWriter output = Misc.getWriter(folder, filename);
    try {
      for (int ij = 0; ij < nprofiles; ij++) {
        output.write("_pd_block_id " + title + "|#" + ij);
        output.newLine();
        output.newLine();
        if (ij == 0) {
          output.write("_diffrn_detector 2D");
          output.newLine();
          output.write("_diffrn_detector_type Image Plate");
          output.newLine();
          output.write("_pd_meas_step_count_time ?");
          output.newLine();
          output.write("_diffrn_measurement_method ?");
          output.newLine();
          output.write("_diffrn_measurement_distance_unit " + imp.getCalibration().getUnit());
          output.newLine();
          output.write("_pd_instr_dist_spec/detc " + radius);
          output.newLine();
          output.write("_diffrn_radiation_wavelength ?");
          output.newLine();
          output.write("_diffrn_source_target ?");
          output.newLine();
          output.write("_diffrn_source_power ?");
          output.newLine();
          output.write("_diffrn_source_current ?");
          output.newLine();
          output.write("_pd_meas_angle_omega " + Double.toString(omega));
          output.newLine();
          output.write("_pd_meas_angle_chi " + Double.toString(chi));
          output.newLine();
          output.write("_pd_meas_angle_phi " + Double.toString(phi));
          output.newLine();
          output.write("_riet_par_spec_displac_x 0");
          output.newLine();
          output.write("_riet_par_spec_displac_y 0");
          output.newLine();
          output.write("_riet_par_spec_displac_z 0");
          output.newLine();
          if (calibrated && radius > 0)
            output.write("_riet_meas_datafile_calibrated true");
          else
            output.write("_riet_meas_datafile_calibrated false");
          output.newLine();
        }
        double eta = (ij - centerIndex) * coneStep;

        output.write("_pd_meas_angle_eta " + Double.toString(eta));
        output.newLine();
        output.newLine();
        output.write("loop_");
        output.newLine();
        output.write(DiffrDataFile.CIFXcoord2T);
        output.newLine();
        output.write(DiffrDataFile.intensityExpCIFstring);
        output.newLine();
//        System.out.println(imp.getCalibration().pixelWidth);
//        System.out.println(imp.getCalibration().pixelHeight);
        for (int i = startX; i < endX; i++) {
          double x = i * coordTrasfX;
          if (calibrated)
            x = (x - getX()) / radius * Constants.PITODEG;
          double intensity = profile[ij][i - startX];
          if (Double.isNaN(intensity))
            intensity = -1;
          if (intensity > 0) {
            output.write(" " + Fmt.format(x) + " " + Fmt.format(intensity));
            output.newLine();
          }
        }
        output.newLine();
      }
    } catch (IOException io) {
    }

    try {
      output.close();
    } catch (IOException io) {
    }
    IJ.wait(250);  // give system time to save the file
    if (filename != null && data != null)
      data.addDataFileforName(folder + filename, false);

  }

}
