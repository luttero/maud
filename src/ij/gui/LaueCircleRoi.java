/*
 * @(#)LaueCircleRoi.java created Aug 8, 2003 Berkeley
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

package ij.gui;

import ij.ImagePlus;
import ij.process.*;
import ij.measure.Calibration;
import it.unitn.ing.rista.util.*;

import java.awt.*;


/**
 * The LaueCircleRoi is a class
 *  
 * @version $Revision: 1.9 $, $Date: 2006/02/02 16:11:56 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class LaueCircleRoi extends OpenRoi {
  double radius = 194.71964;
//  double pixelSize = 1.0;
  double centerX = 0.0;
  double centerY = 0.0;
  double diameter = 0.0;
//  double coordTrasf; // = pixelSize * MoreMath.InchesTomm;
  public double startingAngle = 0.0;
  public double finalAngle = 360.0;
  public double Sx = 0.0;
  public double Sy = 0.0;

  // Creates a new OvalRoi. The ImagePlus argument can be null.
  public LaueCircleRoi(ImagePlus imp, double radius) {
    this(MaudPreferences.getDouble("image2D.centerX", 50.0),
            MaudPreferences.getDouble("image2D.centerY", 50.0),
            MaudPreferences.getDouble("image2D.roiCircle",
                    10),
            imp);
    setRadius(radius);
  }

  // Creates a new OvalRoi. The ImagePlus argument can be null.
  public LaueCircleRoi(double x, double y, double diameter, ImagePlus imp) {
    super(0, 0, imp.getWidth(), imp.getHeight(), imp);
    type = CIRCLE;
//    Calibration cal = imp.getCalibration();
//    pixelSize = cal.pixelWidth;
//    coordTrasf = pixelSize; // * MoreMath.InchesTomm;
    this.centerX = x;
    this.centerY = y;
    this.diameter = diameter;
  }

  // Starts the process of creating a user-defined OvalRoi.
  public LaueCircleRoi(int x, int y, ImagePlus imp) {
    super(x, y, imp);
    type = CIRCLE;
  }

  static int CIRCLE = -998;

  public int getType() {
    return CIRCLE;
  }

  public void updateSelection() {
    // updateClipRect();
    imp.updateAndRepaintWindow();
    //imp.draw(); // clipX, clipY, clipWidth, clipHeight);
    oldX = x;
    oldY = y;
    oldWidth = width;
    oldHeight = height;
  }

	public void setCenterX(double centerX) {
		this.centerX = centerX;
	}

	public void setCenterY(double centerY) {
		this.centerY = centerY;
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

  public double getPixelCircleX() {
    return diameter / imp.getCalibration().pixelWidth;
  }

  public double getPixelCircleY() {
    return diameter / imp.getCalibration().pixelHeight;
  }

  public double getX() {
    return centerX;
  }

  public double getY() {
    return centerY;
  }

  public double getX(int pixel) {
    return imp.getCalibration().getX(pixel);
  }

  public double getY(int pixel) {
    return imp.getCalibration().getY(pixel);
  }

  public int getPixelX(double x) {
    Calibration cal = imp.getCalibration();
    return (int) (x / cal.pixelWidth + cal.xOrigin);
  }

  public int getPixelY(double y) {
    Calibration cal = imp.getCalibration();
    return (int) (y / cal.pixelHeight + cal.yOrigin);
  }

  public double getPixelXd(double x) {
    Calibration cal = imp.getCalibration();
    return x / cal.pixelWidth + cal.xOrigin;
  }

  public double getPixelYd(double y) {
    Calibration cal = imp.getCalibration();
    return y / cal.pixelHeight + cal.yOrigin;
  }

  public void setCircle(double diameter) {
    this.diameter = diameter;
  }

  public void setStartingPoint(double xNew, double yNew) {
    centerX = xNew;
    centerY = yNew;
    startX = (int) xNew;
    startY = (int) yNew;
  }

/*  protected void updateClipRect() {
    // Finds the union of current and previous roi
    clipX = 0;
    clipY = 0;
    clipWidth = xMax;
    clipHeight = yMax;
  }

  public Rectangle getBounds() {
    return new Rectangle(0, 0, xMax, yMax);
  }*/

  /** Draws an outline of this OvalRoi on the image. */
/*  public void draw(Graphics g) {
    g.setColor(ROIColor);
//    double mag = ic.getMagnification();
    g.drawArc(ic.screenX(getPixelX(getX() - getCircle())),
            ic.screenY(getPixelY(getY() - getCircle())),
            ic.screenX(getPixelX(getCircle() * 2.0)), ic.screenY(getPixelY(getCircle() * 2.0)),
            (int) startingAngle, (int) (finalAngle - startingAngle));
    if (updateFullWindow) {
      updateFullWindow = false;
      imp.draw();
    }
    showStatus();
  }*/

  public void draw(Graphics g) {
    if (ic==null) return;
/*    if (usableRectangle != null) {
      int y1 = (int) usableRectangle.getMinY();
      int yMax = (int) usableRectangle.getMaxY();
      int x1 = (int) usableRectangle.getMinX();
      int xMax = (int) usableRectangle.getMaxX();
      g.setColor(Color.GREEN);
      g.drawRect(ic.screenX(x1), ic.screenY(y1), ic.screenX(xMax-x1), ic.screenY(yMax-y1));
    }*/
    g.setColor(Color.RED);
    ROIColor = Color.RED;
    g.setColor(ROIColor);
    mag = ic!=null?ic.getMagnification():1.0;
    int sw = ic.screenX(getPixelX(Math.tan(getCircle() * Math.PI / 180.0) * getRadius()/*mag*/ * 2.0));
    int sh = ic.screenY(getPixelY(Math.tan(getCircle() * Math.PI / 180.0) * getRadius()/*mag*/ * 2.0));
//    int sw2 = (int)(0.14645*width*mag);
//    int sh2 = (int)(0.14645*height*mag);
    int sx1 = ic.screenX(getPixelX(getX() - Math.tan(getCircle() * Math.PI / 180.0) * getRadius()));
    int sy1 = ic.screenY(getPixelY(getY() - Math.tan(getCircle() * Math.PI / 180.0) * getRadius()));
/*    int sx2 = ic.screenX(getPixelX(getCircle() * 2.0));
    int sy2 = ic.screenY(getPixelY(getCircle() * 2.0));
    int sx3 = sx1+sw;
    int sy3 = sy1+sh;*/
//    System.out.println(sx1+" "+sy1+" "+sw+" "+sh+" "+(int) startingAngle+" "+(int) (finalAngle - startingAngle));
    g.drawArc(sx1, sy1, sw, sh, (int) startingAngle, (int) (finalAngle - startingAngle));
/*    if (state!=CONSTRUCTING && clipboard==null) {
      int size2 = HANDLE_SIZE/2;
      drawHandle(g, sx1+sw2-size2, sy1+sh2-size2);
      drawHandle(g, sx3-sw2-size2, sy1+sh2-size2);
      drawHandle(g, sx3-sw2-size2, sy3-sh2-size2);
      drawHandle(g, sx1+sw2-size2, sy3-sh2-size2);
      drawHandle(g, sx2-size2, sy1-size2);
      drawHandle(g, sx3-size2, sy2-size2);
      drawHandle(g, sx2-size2, sy3-size2);
      drawHandle(g, sx1-size2, sy2-size2);
    }*/
    drawPreviousRoi(g);
    if (updateFullWindow) {
      updateFullWindow = false;
      imp.draw();
    }
    if (state!=NORMAL) showStatus();
  }

/*  public double getXCoord(int xcoord) {
    return getX(xcoord) + getX();
  }

  public double getYCoord(int ycoord) {
    return getY(ycoord) + getY();
  }*/

/*  public void drawPixels() {
    // equation for an ellipse is x^2/a^2 + y^2/b^2 = 1
		ImageProcessor ip = imp.getProcessor();
		int a = (int) (getPixelCircleX());
		int b = (int) (getPixelCircleY());
		double a2 = a*a;
		double b2 = b*b;
		int xbase = getPixelX(getX() - getCircle())+a;
		int ybase = getPixelY(getY() - getCircle())+b;
		double yy;
		ip.moveTo(x, y+b);
		for (int i=-a+1; i<=a; i++) {
			yy = Math.sqrt(b2*(1.0-(i*i)/a2));
			ip.lineTo(xbase+i, ybase+(int)(yy+0.5));
		}
		ip.moveTo(x, y+b);
		for (int i=-a+1; i<=a; i++) {
			yy = Math.sqrt(b2*(1.0-(i*i)/a2));
			ip.lineTo(xbase+i, ybase-(int)(yy+0.5));
		}
		if (Line.getWidth()>1)
			updateFullWindow = true;
  }*/

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
    wand.autoOutline(getCircleI()/2,getCircleI()/2, 255, 255);
        for (int i=0; i<wand.npoints; i++) {
            wand.xpoints[i] += x;
            wand.ypoints[i] += y;
        }
    return new Polygon(wand.xpoints, wand.ypoints, wand.npoints);
  }

  public int getCircleI() {
    return (int) (Math.tan(getCircle() * Math.PI / 180.0) * getRadius());
  }


  /** Returns a handle number if the specified screen coordinates are
    inside or near a handle, otherwise returns -1. */
  public int isHandle(int sx, int sy) {
    if (clipboard!=null || ic==null) return -1;
//    double mag = ic.getMagnification();
    int size = HANDLE_SIZE+3;
    int halfSize = size/2;
    int sx1 = ic.screenX(x) - halfSize;
    int sy1 = ic.screenY(y) - halfSize;
    int sx3 = ic.screenX(x+getCircleI()) - halfSize;
    int sy3 = ic.screenY(y+getCircleI()) - halfSize;
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

/*  public int[] getMask() {
    Image img = GUI.createBlankImage((int) (getPixelCircleX()), (int) (getPixelCircleY()));
    Graphics g = img.getGraphics();
    g.setColor(Color.black);
    g.drawOval(getPixelX(getX() - getCircle()), getPixelY(getY() - getCircle()),
            (int) (getPixelCircleX() * 2.0), (int) (getPixelCircleY() * 2.0));
    g.dispose();
    ColorProcessor cp = new ColorProcessor(img);
    return (int[]) cp.getPixels();
  }*/

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

  public double[][] getPixels() {
    int npoints = (int) getPixelCircleX(); // xMax / 2;
    return getPixels(startingAngle, finalAngle, npoints, 0, xMax, 0, yMax, imp.getCalibration().pixelWidth);

  }

  public double[][] getPixels(double startAngle, double endAngle, int npoints,
                            int xMin, int xMax, int yMin, int yMax, double step) {
    ImageProcessor ip = imp.getProcessor();
    if (ip == null) {
      System.out.println("No image processor!");
      return null;
    }
//	  Roi selroi = usableRoi;
//	  int roiType = selroi.getType();

//    int maxGray = 256;
    double oldstartingAngle = startingAngle;
    double oldfinalAngle = finalAngle;
	  startingAngle = startAngle;
	  finalAngle = endAngle;
    double oldCircle = getCircle();

    double[][] profile = new double[1][npoints];

//    double imageIntegrationStep = MaudPreferences.getDouble("", Constants.PI2 / 360.0);
//    int counter = 0;
    double calPixelW = imp.getCalibration().pixelWidth;
    double calPixelH = imp.getCalibration().pixelHeight;
//	  System.out.println(calPixelW + " " + calPixelH);
    double coordX = getX() / calPixelW;
    double coordY = getY() / calPixelH;
//	  System.out.println(getX() + " " + getY() + " " + coordX + " : " + coordY + " " + step + " " + npoints);
    for (int ix = 0; ix < npoints; ix++) {
      profile[0][ix] = 0.0;
      double numbAvg = 0;

      double imageIntegrationStep = 360.0 / (Constants.PI2 * (ix + 1));
      setCircle(ix * step);

	    double dixx = ix * step / calPixelW;
	    double dixy = dixx * calPixelW / calPixelH;
//	    System.out.println(ix + " " + x1 + ":  " + y1);
      for (double iy = startingAngle; iy < finalAngle; iy += imageIntegrationStep) {
//        double diy = iy * calPixelH;
        double arg = iy * Constants.DEGTOPI;
        double x1 = Math.cos(arg) * dixx + coordX;
        double y1 = -Math.sin(arg) * dixy + coordY;

//        System.out.println(ix + ", " + x1 + " : " + y1);

/*        if ((usableRoi != null && usableRoi.contains((int) x1, (int) y1)) ||
		        (usableRoi == null && (x1 >= xMin && x1 < xMax && y1 >= yMin && y1 < yMax))) {
//          x1 *= coordTrasf;
//          y1 *= coordTrasf;
          profile[0][ix] += ip.getInterpolatedPixel(x1, y1);
//      System.out.println("x:  " + x1 + ", y: " + y1 + ", int: " + profile[0][ix]);
          numbAvg++;
        } else {
          profile[0][ix] = Double.NaN;
        }*/

        int i1 = (int) x1;
        int j1 = (int) y1;
        double intensity = 0;
        double count = 0;
        for (int ij = i1 - 1; ij <= i1 + 2; ij++) {
        	 for (int ji = j1 - 1; ji <= j1 + 2; ji++) {
	          if ((usableRoi != null && usableRoi.contains(ij, ji)) ||
			          (usableRoi == null && (ij >= xMin && ij < xMax && ji >= yMin && ji < yMax))) {
					double value = ip.getPixelValue(ij, ji);
					double weight = 0;
					if (value >= 0) {
						double dx = x1 - ij;
						dx *= dx;
						double dy = y1 - ji;
						dy *= dy;
						double distance = Math.sqrt(dx + dy);
						weight = Math.exp(-distance);
						intensity += value * weight;
						count += weight;
					}
	          }
        	 }
        }
        if (count > 0) {
	        profile[0][ix] += intensity / count;
	        numbAvg += 1;
        } else
	        profile[0][ix] = Double.NaN;
// System.out.println("Not x:  " + x1 + " " + iy);
      }
      if (numbAvg != 0 && !Double.isNaN(profile[0][ix])) {
        profile[0][ix] /= numbAvg;
//        if (profile[ix] > maxGray) maxGray *= 256;
      } else
        profile[0][ix] = Double.NaN;

    }

    startingAngle = oldstartingAngle;
    finalAngle = oldfinalAngle;
    setCircle(oldCircle);

/*    for (int ix = 0; ix < npoints; ix++) {
      if (profile[0][ix] < 0.0)
        profile[0][ix] = 0.0;
//      else if (Constants.macosx)
//        profile[ix] = maxGray - profile[ix];
    }*/
//      System.out.println(profile[ix] + " " + maxGray);
    return profile;

  }

}
