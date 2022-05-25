/*
 * @(#)BrukerImageReader.java created Jul 7, 2009 Caen
 *
 * Copyright (c) 2009 Luca Lutterotti All Rights Reserved.
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

import ij.io.FileInfo;
import ij.io.OpenDialog;
import ij.*;
import ij.measure.Calibration;
import ij.process.*;

import java.io.*;
import java.util.Properties;
import java.util.StringTokenizer;
import java.awt.image.ColorModel;

import it.unitn.ing.rista.util.*;

/**
 * The BrukerImageReader is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jul 7, 2009 5:49:12 PM $
 * @since JDK1.1
 */
public class BrukerImageReader implements PlugIn {
  int width = 512;
  int height = 512;
  static int offset = 1;
  int nImages = 1;
  int gapBetweenImages = 0;
  boolean whiteIsZero = true;
  boolean intelByteOrder = true;
  int fileType = FileInfo.GRAY32_UNSIGNED;
  int[] underOverFlow = new int[3];
  int versionNumber = 1;
  int formatNumber = 86;
  int bytePerPixel = 1;
  float pixelWidth = 0.055f;
  float pixelHeight = 0.055f;
  float detectorDistance = 150;
  float cmtogrid = 0;
  float theta2 = 30;
  float omega = 0;
  float phi = 0;
  float chi = 0;
  float centerX = 0;
  float centerY = 0;
  String[] detectorTypes = {"MULTIWIRE", "CCD-PXL-2K", "CCD-PXL-ARR", "CCD-PXL-KAF1500",
      "CCD-PXL-L6500N", "CCD-PXL-L6500F", "CCD-PXL-L6000", "CCD-PXL-KAF2", "CCD-PXL-KAF",
      "CCD-PXL-MSPD", "CCD-PXL", "CCD-LDI-APEX2", "CCD-LDI", "CCD-MMX", "UNKNOWN", "OTHER"};

  float[] pixPerCmType = {47.5f, 56.02f, 32.0f, 51.2f, 32.0f, 32.0f, 56.02f, 81.92f, 81.92f,
      81.92f, 81.92f, 166.6666667f, 83.333333f, 1, 1, 1};
  float[] cmToGridType = {2.0f, 0.8f, 0.4f, 0.8f, 1.5f, 1.5f, 0.3f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 0.8f, 1, 1, 1};

  String detectorType = "MULTIWIRE";
  double pixPerCm = 0;
  double radiusToCut = 1E150;

  double alpha1 = 1.5405981;
  double alpha2 = 1.544497;
  double ratio12 = 0.5;
  double exposureTime = 1;
  String targetMaterial = "Cu";

  String dataType = "u32";

  public static String x_dimension = "NCOLS";
  public static String y_dimension = "NROWS";
  public static String pixel_size = "PIXPERCM";
  public static String pixel512_size = "PIX512PERCM";
  public static String pixel_x_size = "PixelWidth";
  public static String pixel_y_size = "PixelHeight";
  public static String byte_per_pixels = "NPIXELB";
  public static String detector_distance = "DISTANC";
  public static String cm_to_grid = "CMTOGRID";
  public static String center = "CENTER";
  public static String angles_all = "ANGLES";
  public static String overflow = "NOVERFL";
  public static String det_type = "DETTYPE";
  public static String center_x = "centerX";
  public static String center_y = "centerY";
  public static String theta2String = "2theta";
  public static String omegaString = "omega";
  public static String chiString = "chi";
  public static String phiString = "phi";
  public static String etaString = "eta";
  public static String brukerImage = "BrukerImage";
  public static String radiusToCutString = "radiusToCut";

  public static String data_type = "Data type";


  public void run(final String string) {
    if (string.equals("about"))
      showAbout();
    else {
      OpenDialog opendialog
          = new OpenDialog("Open Bruker image", null);
      String string_1_ = opendialog.getDirectory();
      String string_2_ = opendialog.getFileName();
      if (string_2_ != null) {
        File file = new File(string_1_ + string_2_);
        float[] turnedBuffer = loadImage(file, null, null, null);
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
//          fileinfo.pixelDepth = pixelDepth;
        fileinfo.unit = "mm";
        if (IJ.debugMode)
          IJ.write("ImportDialog: " + fileinfo);
        open(true, turnedBuffer, fileinfo);
/*        try {
          DataInputStream dis = new DataInputStream(new FileInputStream(file));
          BufferedReader bis = new BufferedReader(new InputStreamReader(dis));
          byte[] format = new byte[240];
          dis.read(format);
          String formatString = new String(format);
          if (formatString.startsWith("FORMAT :")) { // it is probably a Bruker format
            StringTokenizer st = new StringTokenizer(formatString, ": ");
            String token = "";
            while (!st.nextToken().equalsIgnoreCase("FORMAT")) {}
            formatNumber = Integer.parseInt(st.nextToken());
            while (!st.nextToken().equalsIgnoreCase("VERSION")) {}
            versionNumber = Integer.parseInt(st.nextToken());
            while (!st.nextToken().equalsIgnoreCase("HDRBLKS")) {}
            int numberOfBlocks = Integer.parseInt(st.nextToken());
            byte[] header = new byte[numberOfBlocks * 512 - 240];
            dis.read(header);
            String headerString = new String(header);
            decode(headerString);

            if (pixPerCm == -1)
              for (int i = 0; i < detectorTypes.length; i++)
                if (detectorType.equalsIgnoreCase(detectorTypes[i]))
                  pixPerCm = pixPerCmType[i];
            if (cmtogrid == -1)
              for (int i = 0; i < detectorTypes.length; i++)
                if (detectorType.equalsIgnoreCase(detectorTypes[i]))
                  cmtogrid = cmToGridType[i];
            pixelWidth = (float) (10.0 * 512 / width / (pixPerCm));
            pixelHeight = (float) (10.0 * 512 / height / (pixPerCm));

            detectorDistance += cmtogrid;

          int totalSize = height * width;
          float[] floatBuffer = new float[totalSize];
          java.nio.ByteBuffer bb = java.nio.ByteBuffer.allocate(totalSize * bytePerPixel);
//          System.out.println("Available: " + dis.available() + " to read:" + totalSize * bytePerPixel);
          int numRead = dis.read(bb.array());
            bb.order(java.nio.ByteOrder.LITTLE_ENDIAN);
            switch (bytePerPixel) {
              case 1:
                byte[] buffer = new byte[width * height];
                bb.asReadOnlyBuffer().get(buffer);
                for (int i = 1; i < totalSize; i++) {
                  short value = buffer[i];
                  if (value < 0)
                    value += 256;
                  floatBuffer[i] = value;
                }
                break;
              case 2:
                short[] shortBuffer = new short[width * height];
                bb.asShortBuffer().get(shortBuffer);
                for (int i = 1; i < totalSize; i++)
                  floatBuffer[i] = shortBuffer[i];
                break;
              case 4:
                int[] intBuffer = new int[width * height];
                bb.asIntBuffer().get(intBuffer);
                for (int i = 1; i < totalSize; i++)
                  floatBuffer[i] = intBuffer[i];
                break;
              default:{}
            }

            // reading overflow

            if (underOverFlow[0] > 0) {
              
            }
            if (underOverFlow[1] > 0) {
              if (formatNumber == 86) {
                format = new byte[underOverFlow[1] * 16];
                dis.read(format);
                formatString = new String(format);
                st = new StringTokenizer(formatString, " ");
//                int index = 0;
                for (int i = 0; i < underOverFlow[1]; i++) {
                  int value = Integer.parseInt(st.nextToken());
                  int address = Integer.parseInt(st.nextToken());
                  floatBuffer[address] = value;
                }
              } else {
                
              }
            }
            if (underOverFlow[2] > 0) {

            }

            float min = 500000;
            float max = -500000;
            for (int i = 1; i < totalSize; i++) {
              if (floatBuffer[i] < min)
                min = floatBuffer[i];
              if (floatBuffer[i] > max)
                max = floatBuffer[i];
            }
//            System.out.println("Min and max :" + min + " " + max + " " + lowZero + " " + underOverFlow[1]);

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
//          fileinfo.pixelDepth = pixelDepth;
          fileinfo.unit = "mm";
          if (IJ.debugMode)
            IJ.write("ImportDialog: " + fileinfo);

          dis.close();
            // we turn the image 180 degs

            float normalize = 1;
            if (max > Short.MAX_VALUE)
              normalize = Short.MAX_VALUE / (max * 1.0001f);
            short[] turnedBuffer = new short[floatBuffer.length];
            for (int i = 0; i < floatBuffer.length; i++)
              turnedBuffer[i] = (short) (floatBuffer[floatBuffer.length - i - 1] / normalize);

            // check for circular or square image
            if (MaudPreferences.getBoolean("image.cutValuesOutOfMaxRadius", true)) {

            int maxDistance = (int) Math.sqrt(width * width / 4 + height * height / 4) + 1;
            maxDistance++;
            boolean allZeros = true;
            double cX = 0.5 * width;
            double cY = 0.5 * height;
            double maxZerosPermitted = MaudPreferences.getDouble("image.maxZerosPermitted", 0.05);
            while (allZeros && maxDistance > cX && maxDistance > cY) {
              maxDistance--;
              double maxD2 = maxDistance * maxDistance;
              double zero = 0;
              double nonzero = 0;
              for (int i = 0; i < width; i++) {
                for (int j = 0; j < height; j++) {
                  double dx = cX - i + 1;
                  double dy = cY - j + 1;
                  if (dx * dx + dy * dy > maxD2)
                    if (turnedBuffer[i * height + j] > 0)
                      nonzero += turnedBuffer[i * height + j];
                    else
                      zero++;
                }
              }
              if (nonzero != 0 && zero / nonzero < maxZerosPermitted)
                allZeros = false;
            }
            radiusToCut = maxDistance;
            System.out.println("radius: " + maxDistance);
          }
            
          } else {
            System.out.println("File is not a Bruker format");
            dis.close();
          }
        } catch (Exception e) {
          e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }*/
      }
    }
  }

  boolean decode(String header) {
    StringTokenizer st = new StringTokenizer(header, ": ");
    String headerLine = "";
    while (st.hasMoreTokens()) {
      headerLine = st.nextToken();
      if (headerLine.startsWith(x_dimension)) {
      width = Integer.parseInt(st.nextToken());
    } else if (headerLine.startsWith(y_dimension)) {
      height = Integer.parseInt(st.nextToken());
    } else if (headerLine.startsWith(pixel_size)) {
      pixPerCm = Double.parseDouble(st.nextToken());
    } else if (headerLine.startsWith(pixel512_size)) {
      pixPerCm = Double.parseDouble(st.nextToken());
    } else if (headerLine.startsWith(byte_per_pixels)) {
      bytePerPixel = Integer.parseInt(st.nextToken());
    } else if (headerLine.startsWith(overflow)) {
        if (versionNumber >= 11) {
          underOverFlow[0] = Integer.parseInt(st.nextToken());
          underOverFlow[1] = Integer.parseInt(st.nextToken());
          underOverFlow[2] = Integer.parseInt(st.nextToken());
        } else
          underOverFlow[1] = Integer.parseInt(st.nextToken());
    } else if (headerLine.startsWith(detector_distance)) {
      detectorDistance = Float.parseFloat(st.nextToken());
    } else if (headerLine.startsWith(angles_all)) {
      theta2 = Float.parseFloat(st.nextToken());
        omega = Float.parseFloat(st.nextToken());
        phi = Float.parseFloat(st.nextToken());
        chi = Float.parseFloat(st.nextToken());
    } else if (headerLine.startsWith(center)) {
      centerX = Float.parseFloat(st.nextToken());
      centerY = Float.parseFloat(st.nextToken());
    } else if (headerLine.startsWith(cm_to_grid)) {
      cmtogrid = Float.parseFloat(st.nextToken());
    } else if (headerLine.startsWith(det_type)) {
      detectorType = st.nextToken();
    }
    }
    return true;
  }

  /**
   * Opens the image. Displays it if 'show' is
   * true. Returns an ImagePlus object if successful.
   */
  public void open(boolean show, float[] buffer, FileInfo fi) {
    ImagePlus imp;
    ImageProcessor ip;
    ip = new FloatProcessor(width, height, buffer, null);
    imp = new ImagePlus(fi.fileName, ip);
    imp.setFileInfo(fi);
    setCalibration(imp, fi);
    if (fi.info != null)
      imp.setProperty("Info", fi.info);
    setProperties(imp);
    if (show) imp.show();
    IJ.showProgress(1.0);
  }

  public float[] loadImage(File file, int[] pixelDimension, double[] dimension, String[] properties) {
    float[] turnedBuffer = null;
    try {
      DataInputStream dis = new DataInputStream(new FileInputStream(file));
      BufferedReader bis = new BufferedReader(new InputStreamReader(dis));
      byte[] format = new byte[240];
      dis.read(format);
      String formatString = new String(format);
      if (formatString.startsWith("FORMAT :")) { // it is probably a Bruker format
        StringTokenizer st = new StringTokenizer(formatString, ": ");
        String token = "";
        while (!st.nextToken().equalsIgnoreCase("FORMAT")) {}
        formatNumber = Integer.parseInt(st.nextToken());
        while (!st.nextToken().equalsIgnoreCase("VERSION")) {}
        versionNumber = Integer.parseInt(st.nextToken());
        while (!st.nextToken().equalsIgnoreCase("HDRBLKS")) {}
        int numberOfBlocks = Integer.parseInt(st.nextToken());
        byte[] header = new byte[numberOfBlocks * 512 - 240];
        dis.read(header);
        String headerString = new String(header);
        decode(headerString);

        if (pixPerCm == -1)
          for (int i = 0; i < detectorTypes.length; i++)
            if (detectorType.substring(0, detectorTypes[i].length()).equalsIgnoreCase(detectorTypes[i]))
              pixPerCm = pixPerCmType[i];
        if (cmtogrid == -1)
          for (int i = 0; i < detectorTypes.length; i++)
            if (detectorType.substring(0, detectorTypes[i].length()).equalsIgnoreCase(detectorTypes[i]))
              cmtogrid = cmToGridType[i];
        if (width == 512) {
          pixelWidth = (float) (10.0 / (pixPerCm));
          pixelHeight = (float) (10.0 / (pixPerCm));
        } else {
          pixelWidth = (float) (5.0 / (pixPerCm));
          pixelHeight = (float) (5.0 / (pixPerCm));
        }

        detectorDistance += cmtogrid;

      int totalSize = height * width;
      float[] floatBuffer = new float[totalSize];
      java.nio.ByteBuffer bb = java.nio.ByteBuffer.allocate(totalSize * bytePerPixel);
          System.out.println("Available: " + dis.available() + " to read:" + totalSize * bytePerPixel + ", bytes: " + bytePerPixel);
      int numRead = dis.read(bb.array());
        bb.order(java.nio.ByteOrder.LITTLE_ENDIAN);
        switch (bytePerPixel) {
          case 1:
            byte[] buffer = new byte[width * height];
            bb.asReadOnlyBuffer().get(buffer);
            for (int i = 1; i < totalSize; i++) {
              short value = buffer[i];
              if (value < 0)
                value += 256;
              floatBuffer[i] = value;
            }
            break;
          case 2:
            short[] shortBuffer = new short[width * height];
            bb.asShortBuffer().get(shortBuffer);
            for (int i = 1; i < totalSize; i++)
	            if (shortBuffer[i] > 0)
		            floatBuffer[i] = shortBuffer[i];
	            else
		            floatBuffer[i] = 65536 + shortBuffer[i];
	          break;
          case 4:
            int[] intBuffer = new int[width * height];
            bb.asIntBuffer().get(intBuffer);
            for (int i = 1; i < totalSize; i++)
	            if (intBuffer[i] > 0)
                floatBuffer[i] = intBuffer[i];
	          else
		            floatBuffer[i] = 4294967296l + intBuffer[i];
            break;
          default:{}
        }

        // reading overflow

/*            int lowZero = 0;
            for (int i = 1; i < totalSize; i++)
              if (floatBuffer[i] < 0)
                lowZero++;*/

        if (underOverFlow[0] > 0) {

        }
        if (underOverFlow[1] > 0) {
          if (formatNumber == 86) {
            format = new byte[underOverFlow[1] * 16];
            dis.read(format);
            formatString = new String(format);
            st = new StringTokenizer(formatString, " ");
//                int index = 0;
            for (int i = 0; i < underOverFlow[1]; i++) {
              int value = Integer.parseInt(st.nextToken());
              int address = Integer.parseInt(st.nextToken());
              floatBuffer[address] = value;
            }
          } else {

          }
        }
        if (underOverFlow[2] > 0) {

        }

        float min = 500000;
        float max = -500000;
        for (int i = 1; i < totalSize; i++) {
          if (floatBuffer[i] < min)
            min = floatBuffer[i];
          if (floatBuffer[i] > max)
            max = floatBuffer[i];
        }
//            System.out.println("Min and max :" + min + " " + max + " " + lowZero + " " + underOverFlow[1]);

/*      intelByteOrder = true;
      FileInfo fileinfo = new FileInfo();
      fileinfo.fileFormat = 1;
      fileinfo.fileName = file.getName();
      fileinfo.directory = file.getAbsolutePath();
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
//          fileinfo.pixelDepth = pixelDepth;
      fileinfo.unit = "mm";
      if (IJ.debugMode)
        IJ.write("ImportDialog: " + fileinfo);
*/
      dis.close();
        // we turn the image 180 degs

//        float normalize = 1;
//        if (max > Short.MAX_VALUE)
//          normalize = Short.MAX_VALUE / (max * 1.0001f);
        turnedBuffer = new float[floatBuffer.length];
        for (int i = 0; i < floatBuffer.length; i++)
          turnedBuffer[i] = /*(short) (*/floatBuffer[floatBuffer.length - i - 1];// / normalize);

        // check for circular or square image
        if (MaudPreferences.getBoolean("image.cutValuesOutOfMaxRadius", true)) {

        int maxDistance = (int) Math.sqrt(width * width / 4 + height * height / 4) + 1;
        maxDistance++;
        boolean allZeros = true;
        double cX = 0.5 * width;
        double cY = 0.5 * height;
        double maxZerosPermitted = MaudPreferences.getDouble("image.maxZerosPermitted", 0.05);
        while (allZeros && maxDistance > cX && maxDistance > cY) {
          maxDistance--;
          double maxD2 = maxDistance * maxDistance;
          double zero = 0;
          double nonzero = 0;
          for (int i = 0; i < width; i++) {
            for (int j = 0; j < height; j++) {
              double dx = cX - i + 1;
              double dy = cY - j + 1;
              if (dx * dx + dy * dy > maxD2)
                if (turnedBuffer[i * height + j] > 0)
                  nonzero += turnedBuffer[i * height + j];
                else
                  zero++;
            }
          }
          if (nonzero != 0 && zero / nonzero < maxZerosPermitted)
            allZeros = false;
        }
        radiusToCut = maxDistance;
      }
      } else {
        System.out.println("File is not a Bruker format");
        dis.close();
      }
    } catch (Exception e) {
      e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
    if (pixelDimension != null) {
    pixelDimension[0] = width;
    pixelDimension[1] = height;
    }
    if (dimension != null) {
    dimension[0] = pixelWidth;
    dimension[1] = pixelHeight;
      dimension[2] = radiusToCut;
      dimension[5] = theta2;
      dimension[3] = omega;
      dimension[4] = chi - 90;
      dimension[6] = phi;
      dimension[7] = detectorDistance * 10;
      dimension[8] = exposureTime;
      dimension[8] = alpha1;
      dimension[9] = alpha2;
      dimension[10] = ratio12;
      dimension[11] = centerX * pixelWidth;
      dimension[12] = centerY * pixelHeight;
      dimension[13] = formatNumber;
    }
    if (properties != null) {
      if (MaudPreferences.getBoolean("image.cutValuesOutOfMaxRadius", true))
        properties[0] = "true";
      else
        properties[0] = "false";
      properties[1] = targetMaterial;
      properties[2] = dataType;
      properties[3] = detectorType;
    }
    return turnedBuffer;
  }

  void setProperties(ImagePlus imp) {
    setProperty(imp, x_dimension, width);
    setProperty(imp, y_dimension, height);
    setProperty(imp, pixel_x_size, pixelWidth);
    setProperty(imp, pixel_y_size, pixelHeight);
    imp.setProperty(data_type, dataType);
    setProperty(imp, detector_distance, detectorDistance * 10);
    setProperty(imp, center_x, centerX * pixelWidth);
    setProperty(imp, center_y, centerY * pixelHeight);
    setProperty(imp, omegaString, omega);
    setProperty(imp, theta2String, theta2);
    setProperty(imp, chiString, chi - 90);
    setProperty(imp, phiString, phi);
    setProperty(imp, brukerImage, formatNumber);
    if (MaudPreferences.getBoolean("image.cutValuesOutOfMaxRadius", true))
      setProperty(imp, radiusToCutString, radiusToCut);
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
	    System.out.println("Setting calibration -32768");
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
        ("About Bruker Image Reader...",
            "This plugin reads image in Bruker format as with FrmUtilities.");
  }


}
