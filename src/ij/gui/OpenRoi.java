package ij.gui;

import ij.IJ;
import ij.ImagePlus;
import ij.measure.Calibration;
import it.unitn.ing.rista.util.MaudPreferences;

import java.awt.*;
import java.awt.event.KeyEvent;
import java.util.Locale;

/** A open region of interest and superclass for the other ROI classes. */
public class OpenRoi extends Roi {

  public static final int OPENROI = 100; // Type
  Roi usableRoi = null;

  /** Creates a new rectangular Roi. The ImagePlus argument can be null. */
  public OpenRoi(int x, int y, int width, int height, ImagePlus imp) {
    super(x, y, width, height, imp);
    Calibration cal = imp.getCalibration();
    String unit = MaudPreferences.getPref("images.preferredUnit", "mm");
    double scale = getScalingForUnit(cal.getUnit(), unit);
    if (unit.equals("") || unit.equalsIgnoreCase("none") || cal.pixelWidth==0.0) {
      cal.setUnit(null);
      cal.pixelWidth = 1.0;
      cal.pixelHeight = 1.0;
    } else {
      cal.setUnit(unit);
      cal.pixelWidth = cal.pixelWidth * scale;
      cal.pixelHeight = cal.pixelHeight * scale;
      cal.pixelDepth = cal.pixelDepth * scale;
    }
    imp.repaintWindow();
    state = NORMAL;
    type = OPENROI;
    usableRoi = imp.getRoi();
  }

  /** Starts the process of creating a user-defined rectangular Roi. */
  public OpenRoi(int x, int y, ImagePlus imp) {
    super(x, y, imp);
    Calibration cal = imp.getCalibration();
    String unit = MaudPreferences.getPref("images.preferredUnit", "mm");
    double scale = getScalingForUnit(cal.getUnit(), unit);
    if (unit.equals("") || unit.equalsIgnoreCase("none") || cal.pixelWidth==0.0) {
      cal.setUnit(null);
      cal.pixelWidth = 1.0;
      cal.pixelHeight = 1.0;
    } else {
      cal.setUnit(unit);
      cal.pixelWidth = cal.pixelWidth * scale;
      cal.pixelHeight = cal.pixelHeight * scale;
      cal.pixelDepth = cal.pixelDepth * scale;
    }
    imp.repaintWindow();
    type = OPENROI;
    usableRoi = imp.getRoi();
  }

  public void setLocation(int x, int y) {
    if (x < 0) x = 0;
    if (y < 0) y = 0;
//		if ((x+width)>xMax) x = xMax-width;
//		if ((y+height)>yMax) y = yMax-height;
    //IJ.write(imp.getTitle() + ": Roi.setlocation(" + x + "," + y + ")");
    this.x = x;
    this.y = y;
    startX = x;
    startY = y;
    oldX = x;
    oldY = y;
    oldWidth = 0;
    oldHeight = 0;
  }

  /** Returns the perimeter length. */
  public double getLength() {
    return 1.0;
  }

  protected void grow(int xNew, int yNew) {
    if (clipboard != null)
      return;
//		if (xNew < 0) xNew = 0;
//		if (yNew < 0) yNew = 0;
    if (constrain) {
      // constrain selection to be square
      int dx, dy, d;
      dx = xNew - x;
      dy = yNew - y;
      if (dx < dy)
        d = dx;
      else
        d = dy;
      xNew = x + d;
      yNew = y + d;
    }
    width = Math.abs(xNew - startX);
    height = Math.abs(yNew - startY);
    x = (xNew >= startX) ? startX : startX - width;
    y = (yNew >= startY) ? startY : startY - height;
/*		if ((x+width) > xMax)
			width = xMax-x;
		if ((y+height) > yMax)
			height = yMax-y;*/
    updateClipRect();
    imp.draw(clipX, clipY, clipWidth, clipHeight);
    oldX = x;
    oldY = y;
    oldWidth = width;
    oldHeight = height;
  }

  void move(int xNew, int yNew) {
    x += xNew - startX;
    y += yNew - startY;
//		if (x < 0) x = 0;
//		if (y < 0) y = 0;
//		if ((x+width)>xMax) x = xMax-width;
//		if ((y+height)>yMax) y = yMax-height;
    startX = xNew;
    startY = yNew;
    updateClipRect();
    imp.draw(clipX, clipY, clipWidth, clipHeight);
    oldX = x;
    oldY = y;
    oldWidth = width;
    oldHeight = height;
  }

  /** Nudge ROI one pixel on arrow key press. */
  public void nudge(int key) {
    switch (key) {
      case KeyEvent.VK_UP:
        y--;
        break;
      case KeyEvent.VK_DOWN:
        y++;
        break;
      case KeyEvent.VK_LEFT:
        x--;
        break;
      case KeyEvent.VK_RIGHT:
        x++;
        break;
    }
    updateClipRect();
    imp.draw(clipX, clipY, clipWidth, clipHeight);
    oldX = x;
    oldY = y;
  }

  /** Nudge lower right corner of rectangular and oval ROIs by
   one pixel based on arrow key press. */
  public void nudgeCorner(int key) {
    if (type > OVAL || clipboard != null)
      return;
    switch (key) {
      case KeyEvent.VK_UP:
        height--;
        break;
      case KeyEvent.VK_DOWN:
        height++;
        break;
      case KeyEvent.VK_LEFT:
        width--;
        break;
      case KeyEvent.VK_RIGHT:
        width++;
        break;
    }
    updateClipRect();
    imp.draw(clipX, clipY, clipWidth, clipHeight);
    oldX = x;
    oldY = y;
  }

  protected void updateClipRect() {
    // Finds the union of current and previous roi
    clipX = 0;
    clipY = 0;
    clipWidth = xMax;
    clipHeight = yMax;
    double mag = ic.getMagnification();
    if (mag < 1.0) {
      clipWidth += (int) (1 / mag);
      clipHeight += (int) (1 / mag);
    }
    if (clipboard != null && type != RECTANGLE) {
      clipWidth += 5;
      clipHeight += 5;
    }
  }

/*  public void setUsableRectangle(Rectangle rect) {
    usableRectangle = rect;
  }*/

  public boolean contains(int x, int y) {
/*    if (usableRectangle != null)
      return usableRectangle.contains(x, y);*/
    if (usableRoi != null)
      return usableRoi.contains(x, y);
    return containsOriginal(x, y);
  }

  public boolean containsOriginal(int x, int y) {
    // equation for an ellipse is x^2/a^2 + y^2/b^2 = 1
    boolean xIn = (x > 0 && x < xMax);
    boolean yIn = (y > 0 && y < yMax);

    return yIn && xIn;
  }

  public static double getScalingForUnit(String oldUnit, String newUnit) {
    double newScale = 1.0;
    int newUnitIndex = getUnitIndex(newUnit);
    int oldUnitIndex = getUnitIndex(oldUnit);
    if (newUnitIndex != oldUnitIndex) {
      double newUnitsPerCm = getUnitsPerCm(newUnitIndex);
      double oldUnitsPerCm = getUnitsPerCm(oldUnitIndex);
      if (oldUnitsPerCm != 0.0 && newUnitsPerCm != 0.0) {
        newScale = newUnitsPerCm / oldUnitsPerCm;
      }
    }
    return newScale;
  }

  public static final int NANOMETER=0, MICROMETER=1, MILLIMETER=2, CENTIMETER=3,
     METER=4, KILOMETER=5, INCH=6, FOOT=7, MILE=8, PIXEL=9, OTHER_UNIT=10;

  public static double getUnitsPerCm(int unitIndex) {
    switch (unitIndex) {
      case NANOMETER: return  10000000.0;
      case MICROMETER: return    10000.0;
      case MILLIMETER: return       10.0;
      case CENTIMETER: return        1.0;
      case METER: return             0.01;
      case KILOMETER: return         0.00001;
      case INCH: return              0.3937;
      case FOOT: return              0.0328083;
      case MILE: return              0.000006213;
      default: return                0.0;
    }
  }

  public static int getUnitIndex(String unit) {
    unit = unit.toLowerCase(Locale.US);
    if (unit.equals("cm")||unit.startsWith("cent"))
      return CENTIMETER;
    else if (unit.equals("mm")||unit.startsWith("milli"))
      return MILLIMETER;
    else if (unit.startsWith("inch"))
      return INCH;
    else if (unit.startsWith(""+ IJ.micronSymbol)||unit.startsWith("u")||unit.startsWith("micro"))
      return MICROMETER;
    else if (unit.equals("nm")||unit.startsWith("nano"))
      return NANOMETER;
    else if (unit.startsWith("meter"))
      return METER;
    else if (unit.equals("km")||unit.startsWith("kilo"))
      return KILOMETER;
    else if (unit.equals("ft")||unit.equals("foot")||unit.equals("feet"))
      return FOOT;
    else if (unit.equals("mi")||unit.startsWith("mile"))
      return MILLIMETER;
    else
      return OTHER_UNIT;
  }

}
