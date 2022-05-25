/*
 * @(#)KCDReader.java created Jun 4, 2007 Caen
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
package ij.plugin;

import ij.*;
import ij.io.FileInfo;
import ij.io.OpenDialog;
import ij.measure.Calibration;
import ij.process.ImageProcessor;
import ij.process.ShortProcessor;
import it.unitn.ing.rista.util.*;

import java.awt.image.ColorModel;
import java.io.*;
import java.util.Properties;

/**
 * The KCDReader is a class to read KappaCCD .kcd image
 * from Nonius Kappa CCD singlecrystal goniometer
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 4, 2007 4:17:45 PM $
 * @since JDK1.1
 */
public class KCDReader implements PlugIn {
  int width = 625;
  int height = 576;
  static int offset = 37;
  int nImages = 1;
  int gapBetweenImages = 0;
  boolean whiteIsZero = false;
  boolean intelByteOrder = false;
  int fileType = FileInfo.GRAY16_UNSIGNED;
  float pixelWidth = 0.055f;
  float pixelHeight = 0.055f;
  float pixelDepth = 5.0f;
  double omegaStart = 0.0;
  double omegaRange = 0.0;
  double omegaInterval = 0.0;
  double kappaStart = 0.0;
  double kappaRange = 0.0;
  double kappaInterval = 0.0;
  double thetaStart = 0.0;
  double thetaRange = 0.0;
  double thetaInterval = 0.0;
  double phiStart = 0.0;
  double phiRange = 0.0;
  double phiInterval = 0.0;
  double dxStart = 60.0;
  double dxRange = 0.0;
  double dxInterval = 0.0;
  boolean binnedMode = true;
  double exposureTime = 10.0;
  String targetMaterial = "MO";
  String dataType = "u16";
  String polarisationDirection = "PERPENDICULAR";
  double alpha1 = 0.709300;
  double alpha2 = 0.713590;
  double ratio12 = 2.0;
  double kappaSupportAngle = 50.00147;
  double beamCatcherDiameter = 1.5;
  double beamCatcherDistance = 6.5;
  public static String x_dimension = "X dimension";
  public static String y_dimension = "Y dimension";
  public static String pixel_x_size = "pixel X-size";
  public static String pixel_y_size = "pixel Y-size";
  public static String omega_start = "Omega start";
  public static String omega_scan_range = "Omega scan range";
  public static String omega_scan_interval = "Omega scan interval";
  public static String kappa_start = "Kappa start";
  public static String kappa_scan_range = "Kappa scan range";
  public static String kappa_scan_interval = "Kappa scan interval";
  public static String theta_start = "Theta start";
  public static String theta_scan_range = "Theta scan range";
  public static String theta_scan_interval = "Theta scan interval";
  public static String phi_start = "Phi start";
  public static String phi_scan_range = "Phi scan range";
  public static String phi_scan_interval = "Phi scan interval";
  public static String dx_start = "Dx start";
  public static String dx_scan_range = "Dx scan range";
  public static String dx_scan_interval = "Dx scan interval";
  public static String binned_mode = "Binned mode";
  public static String exposure_time = "Exposure time";
  public static String target_material = "Target material";
  public static String alpha1_radiation = "Alpha1 ";
  public static String alpha2_radiation = "Alpha2 ";
  public static String alpha1_2_ratio = "Alpha1/Alpha2 ratio";
  public static String polarisation_direction = "Polarisation direction";
  public static String kappa_support_angle = "Kappa-support angle";
  public static String beam_catcher_diameter = "Beam catcher diameter";
  public static String beam_catcher_distance = "Beam catcher distance";
  public static String data_type = "Data type";


  public void run(final String string) {
    if (string.equals("about"))
      showAbout();
    else {
      OpenDialog opendialog
          = new OpenDialog("Open KCD image", null);
      String string_1_ = opendialog.getDirectory();
      String string_2_ = opendialog.getFileName();
      if (string_2_ != null) {
        try {
          File file = new File(string_1_ + string_2_);
          DataInputStream dis = new DataInputStream(new FileInputStream(file));
          BufferedReader bis = new BufferedReader(new InputStreamReader(dis));
          boolean endOfHeaders = false;
          do {
            endOfHeaders = decode(bis.readLine());
          } while (!endOfHeaders);
          int totalSize = (width * height);
          java.nio.ByteBuffer bb = java.nio.ByteBuffer.allocate(dis.available()/*4 * totalSize*/);
          System.out.println(dis.available() + " " + dis.available() / 4);
          System.out.println(totalSize);
          int numRead = dis.read(bb.array());
          short[] buffer = new short[totalSize];
          bb.order(java.nio.ByteOrder.LITTLE_ENDIAN);
          bb.asShortBuffer().get(buffer);
          int min = buffer[0];
          int max = buffer[0];
          for (int i = 1; i < totalSize; i++) {
            if (buffer[i] < min)
              min = buffer[i];
            if (buffer[i] > max)
              max = buffer[i];
          }
//          System.out.println("Image min = " + min + ", max = " + max);
          intelByteOrder = true;
          FileInfo fileinfo = new FileInfo();
          fileinfo.fileFormat = 1;
          fileinfo.fileName = string_2_;
          fileinfo.directory = string_1_;
          fileinfo.width = width;
          fileinfo.height = height;
          fileinfo.offset = offset;
          fileinfo.nImages = nImages;
          fileinfo.gapBetweenImages = gapBetweenImages;
          fileinfo.intelByteOrder = intelByteOrder;
          fileinfo.whiteIsZero = whiteIsZero;
          fileinfo.fileType = fileType;
          fileinfo.pixelWidth = pixelWidth;
          fileinfo.pixelHeight = pixelHeight;
          fileinfo.pixelDepth = pixelDepth;
          fileinfo.unit = "mm";
          if (IJ.debugMode)
            IJ.write("ImportDialog: " + fileinfo);
          dis.close();
          open(true, buffer, fileinfo);
        } catch (Exception e) {
          e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
      }
    }
  }

  boolean decode(String headerLine) {
    if (headerLine.startsWith(x_dimension)) {
      width = Integer.parseInt(headerLine.substring(14));
    } else if (headerLine.startsWith(y_dimension)) {
      height = Integer.parseInt(headerLine.substring(14));
    } else if (headerLine.startsWith(pixel_x_size)) {
      pixelWidth = (float) (Double.parseDouble(headerLine.substring(20)) / 1000);
      if (binnedMode)
        pixelWidth *= 2;
    } else if (headerLine.startsWith(pixel_y_size)) {
      pixelHeight = (float) (Double.parseDouble(headerLine.substring(20)) / 1000);
      if (binnedMode)
        pixelHeight *= 2;
    } else if (headerLine.startsWith(omega_start)) {
      omegaStart = Double.parseDouble(headerLine.substring(14));
    } else if (headerLine.startsWith(omega_scan_range)) {
      omegaRange = Double.parseDouble(headerLine.substring(19));
    } else if (headerLine.startsWith(omega_scan_interval)) {
      omegaInterval = Double.parseDouble(headerLine.substring(22));
    } else if (headerLine.startsWith(kappa_start)) {
      kappaStart = Double.parseDouble(headerLine.substring(14));
    } else if (headerLine.startsWith(kappa_scan_range)) {
      kappaRange = Double.parseDouble(headerLine.substring(19));
    } else if (headerLine.startsWith(kappa_scan_interval)) {
      kappaInterval = Double.parseDouble(headerLine.substring(22));
    } else if (headerLine.startsWith(theta_start)) {
      thetaStart = Double.parseDouble(headerLine.substring(14));
    } else if (headerLine.startsWith(theta_scan_range)) {
      thetaRange = Double.parseDouble(headerLine.substring(19));
    } else if (headerLine.startsWith(theta_scan_interval)) {
      thetaInterval = Double.parseDouble(headerLine.substring(22));
    } else if (headerLine.startsWith(phi_start)) {
      phiStart = Double.parseDouble(headerLine.substring(12));
    } else if (headerLine.startsWith(phi_scan_range)) {
      phiRange = Double.parseDouble(headerLine.substring(17));
    } else if (headerLine.startsWith(phi_scan_interval)) {
      phiInterval = Double.parseDouble(headerLine.substring(20));
    } else if (headerLine.startsWith(dx_start)) {
      dxStart = Double.parseDouble(headerLine.substring(11));
    } else if (headerLine.startsWith(dx_scan_range)) {
      dxRange = Double.parseDouble(headerLine.substring(17));
    } else if (headerLine.startsWith(dx_scan_interval)) {
      dxInterval = Double.parseDouble(headerLine.substring(20));
    } else if (headerLine.startsWith(binned_mode)) {
      binnedMode = true;
    } else if (headerLine.startsWith(exposure_time)) {
      exposureTime = Double.parseDouble(headerLine.substring(16));
    } else if (headerLine.startsWith(target_material)) {
      targetMaterial = headerLine.substring(18);
    } else if (headerLine.startsWith(alpha1_radiation)) {
      alpha1 = Double.parseDouble(headerLine.substring(9));
    } else if (headerLine.startsWith(alpha2_radiation)) {
      alpha2 = Double.parseDouble(headerLine.substring(9));
    } else if (headerLine.startsWith(alpha1_2_ratio)) {
      ratio12 = Double.parseDouble(headerLine.substring(22));
    } else if (headerLine.startsWith(polarisation_direction)) {
      polarisationDirection = headerLine.substring(25);
    } else if (headerLine.startsWith(kappa_support_angle)) {
      kappaSupportAngle = Double.parseDouble(headerLine.substring(22));
    } else if (headerLine.startsWith(beam_catcher_diameter)) {
      beamCatcherDiameter = Double.parseDouble(headerLine.substring(24));
    } else if (headerLine.startsWith(beam_catcher_distance)) {
      beamCatcherDistance = Double.parseDouble(headerLine.substring(24));
    } else if (headerLine.startsWith(data_type)) {
      dataType = headerLine.substring(12);
      return true;
    }
    return false;
  }

  /**
   * Opens the image. Displays it if 'show' is
   * true. Returns an ImagePlus object if successful.
   */
  public void open(boolean show, short[] buffer, FileInfo fi) {
    ImagePlus imp;
    ImageProcessor ip;
    int i;
    if (buffer == null) return;
    short[] sbuffer = new short[buffer.length];
    int shiftedColumn = MaudPreferences.getInteger("kappaCCDImageFormat.columnToShift", 454);
    for (i = 0; i < height; i++)
      for (int j = shiftedColumn; j < width; j++)
        sbuffer[i * width + (j - shiftedColumn)] = buffer[i * width + j];
    for (i = 0; i < height; i++)
      for (int j = 0; j < shiftedColumn; j++)
        sbuffer[i * width + j + width - shiftedColumn] = buffer[i * width + j];
    int shiftedRow = MaudPreferences.getInteger("kappaCCDImageFormat.rowToShift", 573);
    int index = 0;
    for (i = shiftedRow; i < height; i++)
      for (int j = 0; j < width; j++)
        buffer[index++] = sbuffer[i * width + j];
    for (i = 0; i < shiftedRow; i++)
      for (int j = 0; j < width; j++)
        buffer[index++] = sbuffer[i * width + j];
    // now invert x and y and mirror along the new x
/*    short[][] fbuffer = new short[height][width];
    for (i = 0; i < height; i++)
      for (int j = 0; j < width; j++)
        fbuffer[i][j] = buffer[i * width + j];
    for (i = 0; i < width; i++)
      for (int j = 0; j < height; j++)
        buffer[i * height + j] = fbuffer[height - j - 1][width - i - 1];
    int twidth = width;
    width = height;
    height = twidth;
    float tpixel = pixelWidth;
    pixelWidth = pixelHeight;
    pixelHeight = tpixel;*/
    ip = new ShortProcessor(width, height, buffer, null);
    imp = new ImagePlus(fi.fileName, ip);
    imp.setFileInfo(fi);
    setCalibration(imp, fi);
    if (fi.info != null)
      imp.setProperty("Info", fi.info);
    setProperties(imp);
    if (show) imp.show();
    IJ.showProgress(1.0);
  }

  public short[] loadImage(File file, int[] pixelDimension, double[] dimension, String[] properties) {
    short[] buffer = null;
    try {
      DataInputStream dis = new DataInputStream(new FileInputStream(file));
      BufferedReader bis = new BufferedReader(new InputStreamReader(dis));
      String[] headerLines = new String[offset];
      for (int i = 0; i < offset; i++) {
        headerLines[i] = bis.readLine();
//            System.out.println(headerLines[i]);
        decode(headerLines[i]);
      }
      int totalSize = (width * height);
      java.nio.ByteBuffer bb = java.nio.ByteBuffer.allocate(dis.available()/*4 * totalSize*/);
//          System.out.println(dis.available() + " " + dis.available() / 4);
      int numRead = dis.read(bb.array());
      buffer = new short[totalSize];
      bb.order(java.nio.ByteOrder.LITTLE_ENDIAN);
      bb.asShortBuffer().get(buffer);
      int min = buffer[0];
      int max = buffer[0];
      for (int i = 1; i < totalSize; i++) {
        if (buffer[i] < min)
          min = buffer[i];
        if (buffer[i] > max)
          max = buffer[i];
      }
    } catch (IOException e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
    int i;
    if (buffer == null) return null;
    short[] sbuffer = new short[buffer.length];
    int shiftedColumn = MaudPreferences.getInteger("kappaCCDImageFormat.columnToShift", 454);
    for (i = 0; i < height; i++)
      for (int j = shiftedColumn; j < width; j++)
        sbuffer[i * width + (j - shiftedColumn)] = buffer[i * width + j];
    for (i = 0; i < height; i++)
      for (int j = 0; j < shiftedColumn; j++)
        sbuffer[i * width + j + width - shiftedColumn] = buffer[i * width + j];
    int shiftedRow = MaudPreferences.getInteger("kappaCCDImageFormat.rowToShift", 573);
    int index = 0;
    for (i = shiftedRow; i < height; i++)
      for (int j = 0; j < width; j++)
        buffer[index++] = sbuffer[i * width + j];
    for (i = 0; i < shiftedRow; i++)
      for (int j = 0; j < width; j++)
        buffer[index++] = sbuffer[i * width + j];
    // now invert x and y and mirror along the new x <---- 21/11/2012 this create an error
/*    short[][] fbuffer = new short[height][width];
    for (i = 0; i < height; i++)
      for (int j = 0; j < width; j++)
        fbuffer[i][j] = buffer[i * width + j];
    for (i = 0; i < width; i++)
      for (int j = 0; j < height; j++)
        buffer[i * height + j] = fbuffer[height - j - 1][width - i - 1];
    int twidth = width;
    width = height;
    height = twidth;
    float tpixel = pixelWidth;
    pixelWidth = pixelHeight;
    pixelHeight = tpixel;*/
    pixelDimension[0] = width;
    pixelDimension[1] = height;
    dimension[0] = pixelWidth;
    dimension[1] = pixelHeight;
    dimension[2] = kappaSupportAngle;
    dimension[3] = omegaStart + omegaRange / 2;
    dimension[4] = kappaStart + kappaRange / 2;
    dimension[5] = thetaStart * 2 + thetaRange;
    dimension[6] = phiStart + phiRange / 2;
    double[] omegaChiPhi = Angles.eulerianFromKappa(dimension[2], dimension[3], dimension[4], dimension[6]);
    for (i = 0; i < omegaChiPhi.length; i++)
      if (Math.abs(omegaChiPhi[i]) < 1.0E-5)
        omegaChiPhi[i] = 0.0;
    dimension[3] = omegaChiPhi[0];
    dimension[4] = omegaChiPhi[1];
    dimension[6] = omegaChiPhi[2];
    dimension[7] = dxStart + dxRange / 2;
    if (binnedMode)
      properties[0] = "true";
    else
      properties[0] = "false";
    dimension[8] = exposureTime;
    properties[1] = targetMaterial;
    properties[2] = dataType;
    properties[3] = polarisationDirection;
    dimension[8] = alpha1;
    dimension[9] = alpha2;
    dimension[10] = ratio12;
    dimension[11] = kappaSupportAngle;
    dimension[12] = beamCatcherDiameter;
    dimension[13] = beamCatcherDistance;
    return buffer;
  }

  void setProperties(ImagePlus imp) {
    setProperty(imp, x_dimension, width);
    setProperty(imp, y_dimension, height);
    setProperty(imp, pixel_x_size, pixelWidth);
    setProperty(imp, pixel_y_size, pixelHeight);
    setProperty(imp, omega_start, omegaStart);
    setProperty(imp, omega_scan_range, omegaRange);
    setProperty(imp, omega_scan_interval, omegaInterval);
    setProperty(imp, kappa_start, kappaStart);
    setProperty(imp, kappa_scan_range, kappaRange);
    setProperty(imp, kappa_scan_interval, kappaInterval);
    setProperty(imp, theta_start, thetaStart);
    setProperty(imp, theta_scan_range, thetaRange);
    setProperty(imp, theta_scan_interval, thetaInterval);
    setProperty(imp, phi_start, phiStart);
    setProperty(imp, phi_scan_range, phiRange);
    setProperty(imp, phi_scan_interval, phiInterval);
    setProperty(imp, dx_start, dxStart);
    setProperty(imp, dx_scan_range, dxRange);
    setProperty(imp, dx_scan_interval, dxInterval);
    setProperty(imp, binned_mode, binnedMode);
    setProperty(imp, exposure_time, exposureTime);
    imp.setProperty(target_material, targetMaterial);
    setProperty(imp, alpha1_radiation, alpha1);
    setProperty(imp, alpha2_radiation, alpha2);
    setProperty(imp, alpha1_2_ratio, ratio12);
    imp.setProperty(polarisation_direction, polarisationDirection);
    setProperty(imp, kappa_support_angle, kappaSupportAngle);
    setProperty(imp, beam_catcher_diameter, beamCatcherDiameter);
    setProperty(imp, beam_catcher_distance, beamCatcherDistance);
    imp.setProperty(data_type, dataType);
  }

  void setProperty(ImagePlus imp, String key, double value) {
    imp.setProperty(key, Double.toString(value));
  }

  void setProperty(ImagePlus imp, String key, boolean value) {
    imp.setProperty(key, new Boolean(value));
  }

  void setCalibration(ImagePlus imp, FileInfo fi) {
    if (fi.fileType == FileInfo.GRAY16_SIGNED) {
      if (IJ.debugMode) IJ.log("16-bit signed");
      double[] coeff = new double[2];
      coeff[0] = -32768.0;
      coeff[1] = 1.0;
      if (imp.getGlobalCalibration() != null) {
        // signed 16-bit images and global galibration cannot coexist
        imp.setGlobalCalibration(null);
        WindowManager.repaintImageWindows();
        IJ.error("FileOpener", "Global calibration disabled");
      }
      imp.getCalibration().setFunction(Calibration.STRAIGHT_LINE, coeff, "gray value");
    }

    Properties props = decodeDescriptionString(fi);
    Calibration cal = imp.getCalibration();
    if (fi.pixelWidth > 0.0 && fi.unit != null) {
      cal.pixelWidth = fi.pixelWidth;
      cal.pixelHeight = fi.pixelHeight;
      cal.pixelDepth = fi.pixelDepth;
      cal.setUnit(fi.unit);
    }

    if (fi.valueUnit != null) {
      int f = fi.calibrationFunction;
      if ((f >= Calibration.STRAIGHT_LINE && f <= Calibration.LOG2 && fi.coefficients != null)
          || f == Calibration.UNCALIBRATED_OD) {
        boolean zeroClip = props.getProperty("zeroclip", "false").equals("true");
        cal.setFunction(f, fi.coefficients, fi.valueUnit, zeroClip);
      }
    }

    if (fi.frameInterval != 0.0)
      cal.frameInterval = fi.frameInterval;

    if (props == null)
      return;

    cal.xOrigin = getDouble(props, "xorigin");
    cal.yOrigin = getDouble(props, "yorigin");
    cal.zOrigin = getDouble(props, "zorigin");
    cal.info = props.getProperty("info");

    double displayMin = getDouble(props, "min");
    double displayMax = getDouble(props, "max");
    if (!(displayMin == 0.0 && displayMax == 0.0)) {
      int type = imp.getType();
      ImageProcessor ip = imp.getProcessor();
      if (type == ImagePlus.GRAY8 || type == ImagePlus.COLOR_256)
        ip.setMinAndMax(displayMin, displayMax);
      else if (type == ImagePlus.GRAY16 || type == ImagePlus.GRAY32) {
        if (ip.getMin() != displayMin || ip.getMax() != displayMax)
          ip.setMinAndMax(displayMin, displayMax);
      }
    }

    int stackSize = imp.getStackSize();
    if (stackSize > 1) {
      int channels = (int) getDouble(props, "channels");
      int slices = (int) getDouble(props, "slices");
      int frames = (int) getDouble(props, "frames");
      if (channels == 0) channels = 1;
      if (slices == 0) slices = 1;
      if (frames == 0) frames = 1;
      //IJ.log("setCalibration: "+channels+"  "+slices+"  "+frames);
      if (channels * slices * frames == stackSize)
        imp.setDimensions(channels, slices, frames);
    }
  }

  /**
   * Returns an IndexColorModel for the image specified by this FileInfo.
   */
  public ColorModel createColorModel(FileInfo fi) {
    return LookUpTable.createGrayscaleColorModel(fi.whiteIsZero);
  }

  Properties decodeDescriptionString(FileInfo fi) {
    if (fi.description == null || fi.description.length() < 7)
      return null;
    if (IJ.debugMode)
      IJ.log("Image Description: " + new String(fi.description).replace('\n', ' '));
    if (!fi.description.startsWith("ImageJ"))
      return null;
    Properties props = new Properties();
    InputStream is = new ByteArrayInputStream(fi.description.getBytes());
    try {
      props.load(is);
      is.close();
    }
    catch (IOException e) {
      return null;
    }
    fi.unit = props.getProperty("unit", "");
    Double n = getNumber(props, "cf");
    if (n != null) fi.calibrationFunction = n.intValue();
    double c[] = new double[5];
    int count = 0;
    for (int i = 0; i < 5; i++) {
      n = getNumber(props, "c" + i);
      if (n == null) break;
      c[i] = n.doubleValue();
      count++;
    }
    if (count >= 2) {
      fi.coefficients = new double[count];
      for (int i = 0; i < count; i++)
        fi.coefficients[i] = c[i];
    }
    fi.valueUnit = props.getProperty("vunit");
    n = getNumber(props, "images");
    if (n != null && n.doubleValue() > 1.0)
      fi.nImages = (int) n.doubleValue();
    if (fi.nImages > 1) {
      double spacing = getDouble(props, "spacing");
      if (spacing != 0.0)
        fi.pixelDepth = spacing;
      n = getNumber(props, "fps");
      double fps = getDouble(props, "fps");
      if (fps != 0.0)
        fi.frameInterval = 1.0 / fps;
    }
    return props;
  }

  public static Double getNumber(Properties props, String key) {
    String s = props.getProperty(key);
    if (s != null) {
      try {
        return Double.valueOf(s);
      } catch (NumberFormatException e) {
      }
    }
    return null;
  }

  public static double getDouble(Properties props, String key) {
    Double n = getNumber(props, key);
    return n != null ? n.doubleValue() : 0.0;
  }

  void showAbout() {
    IJ.showMessage
        ("About KappaCCD Reader...",
            "This plugin reads image with the \".kcd\" format from Nonius.");
  }

}

