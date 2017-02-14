/*
 * @(#)SISReader.java created Jun 3, 2007 Caen
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
import it.unitn.ing.rista.util.MaudPreferences;
import it.unitn.ing.rista.util.Misc;

import java.awt.image.ColorModel;
import java.io.*;
import java.util.Properties;

/**
 * The SISReader is a class to read SIS images from Hypernex
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 3, 2007 7:33:13 AM $
 * @since JDK1.1
 */
public class SISReader implements PlugIn {
  int width = 2048;
  int height = 2048;
  int offset = 4 * 8;
  int nImages = 1;
  int gapBetweenImages = 0;
  boolean whiteIsZero = false;
  boolean intelByteOrder = false;
  int fileType = FileInfo.GRAY16_UNSIGNED;
  double pixelSize = MaudPreferences.getDouble("SISImage.pixelSize", 0.0703125);

  public void run(final String string) {
    if (string.equals("about"))
      showAbout();
    else {
      OpenDialog opendialog
          = new OpenDialog("Open SIS image", null);
      String string_1_ = opendialog.getDirectory();
      String string_2_ = opendialog.getFileName();
      if (string_2_ != null) {
        try {
          File file = new File(string_1_ + string_2_);
          DataInputStream dis = new DataInputStream(new FileInputStream(file));
          int[] headerBuffer = new int[8];
          java.nio.ByteBuffer bb = java.nio.ByteBuffer.allocate((4 * headerBuffer.length));
          dis.read(bb.array());
          bb.order(java.nio.ByteOrder.LITTLE_ENDIAN);
          bb.asIntBuffer().get(headerBuffer);
          int totalSize = (width * height);
          bb = java.nio.ByteBuffer.allocate((4 * totalSize));
          int numRead = dis.read(bb.array());
          int[] buffer = new int[totalSize];
          bb.order(java.nio.ByteOrder.LITTLE_ENDIAN);
          bb.asIntBuffer().get(buffer);
          int min = buffer[0];
          int max = buffer[0];
          for (int i = 1; i < totalSize; i++) {
            if (buffer[i] < min)
              min = buffer[i];
            if (buffer[i] > max)
              max = buffer[i];
          }
          System.out.println("Min = " + min + ", max = " + max);
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
          fileinfo.pixelWidth = pixelSize;
          fileinfo.pixelHeight = pixelSize;
          fileinfo.pixelDepth = pixelSize;
          fileinfo.unit = "mm";
          if (IJ.debugMode)
            IJ.write("ImportDialog: " + fileinfo);
          dis.close();
          open(true, buffer, fileinfo);
        } catch (Exception e) {
          try {
            pixelSize *= 2;
            width = height = width / 2;
            File file = new File(string_1_ + string_2_);
            DataInputStream dis = new DataInputStream(new FileInputStream(file));
            int[] headerBuffer = new int[8];
            java.nio.ByteBuffer bb = java.nio.ByteBuffer.allocate((4 * headerBuffer.length));
            dis.read(bb.array());
            bb.order(java.nio.ByteOrder.LITTLE_ENDIAN);
            bb.asIntBuffer().get(headerBuffer);
            int totalSize = (width * height);
            bb = java.nio.ByteBuffer.allocate((4 * totalSize));
            int numRead = dis.read(bb.array());
            int[] buffer = new int[totalSize];
            bb.order(java.nio.ByteOrder.LITTLE_ENDIAN);
            bb.asIntBuffer().get(buffer);
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
            fileinfo.pixelWidth = pixelSize;
            fileinfo.pixelHeight = pixelSize;
            fileinfo.pixelDepth = pixelSize;
            fileinfo.unit = "mm";
            if (IJ.debugMode)
              IJ.write("ImportDialog: " + fileinfo);
            dis.close();
            open(true, buffer, fileinfo);
          } catch (Exception ie) {
            ie.printStackTrace();
          }
        }
      }
    }
  }

  /**
   * Opens the image. Displays it if 'show' is
   * true. Returns an ImagePlus object if successful.
   */
  public void open(boolean show, int[] buffer, FileInfo fi) {
    ImagePlus imp;
    ImageProcessor ip;
    if (buffer == null) return;
    short[] sbuffer = new short[buffer.length];
    for (int i = 0; i < width; i++)
      for (int j = 0; j < height; j++)
      sbuffer[i + j * width] = (short) buffer[i + (height - j - 1) * width];
    ip = new ShortProcessor(width, height, sbuffer, null);
    imp = new ImagePlus(fi.fileName, ip);
    imp.setFileInfo(fi);
    setCalibration(imp, fi);
    if (fi.info != null)
      imp.setProperty("Info", fi.info);
    if (show) imp.show();
    IJ.showProgress(1.0);
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

  Double getNumber(Properties props, String key) {
    String s = props.getProperty(key);
    if (s != null) {
      try {
        return Double.valueOf(s);
      } catch (NumberFormatException e) {
      }
    }
    return null;
  }

  double getDouble(Properties props, String key) {
    Double n = getNumber(props, key);
    return n != null ? n.doubleValue() : 0.0;
  }

  void showAbout() {
    IJ.showMessage
        ("About SIS Reader...",
            "This plugin reads image with the \".sis\" format from Hypernex. These can be 1024x1024 or 2048x2048");
  }

}
