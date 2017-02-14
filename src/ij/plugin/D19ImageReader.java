/*
 * @(#)D19ImageReader.java created Jun 16, 2008 Caen
 *
 * Copyright (c) 2008 Luca Lutterotti All Rights Reserved.
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
import java.util.*;

/**
 * The D19ImageReader is a class to read D19 _LAMP image
 * from D19 new neutron diffractometer
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 16, 2008 3:57:39 PM $
 * @since JDK1.1
 */
public class D19ImageReader implements PlugIn {

	int x_size = 640, y_size = 256, z_size = 0;
	int fileType = FileInfo.GRAY16_UNSIGNED;

	public void run(final String string) {
    if (string.equals("about"))
      showAbout();
    else {
      OpenDialog opendialog
          = new OpenDialog("Open D19 image", null);
      String string_1_ = opendialog.getDirectory();
      String string_2_ = opendialog.getFileName();
      if (string_2_ != null) {
        try {
	        double pixelWidth = MaudPreferences.getDouble("d19Detector.pixelX", 2.5);
	        double pixelHeight = MaudPreferences.getDouble("d19Detector.pixelY", 1.5);
	        boolean shift128Pixels = MaudPreferences.getBoolean("d19Detector.shift128Pixel", true);
	        double centerX = MaudPreferences.getDouble("d19Detector.ImageCenterX", -58);
	        double centerY = MaudPreferences.getDouble("d19Detector.ImageCenterY", 194.11);
	        double radius = MaudPreferences.getDouble("d19Detector.detectorSampleDistance", 768.8);

	        double coneInterval = MaudPreferences.getDouble("d19Detector.defaultEtaConeInterval", 5.0);
	        double theta2Step = MaudPreferences.getDouble("d19Detector.defaultDiffractionStepAngle", 0.05);
	        double coneAngleMax = MaudPreferences.getDouble("d19Detector.defaultEtaConeAngleMax", 180.0);

	        int[] imageDimension = new int[3];
	        Vector<double[]> dimension = new Vector<double[]>();
	        String[] properties = new String[1];

	        Vector allData = loadImage(string_1_, string_2_, imageDimension, dimension, properties);

	        int pixelDepth = 1;
	        int width = imageDimension[0];
	        int height = imageDimension[1];
	        int totalSize = width * height;
	        short[] buffer = new short[totalSize];
	        int index = 0;
	        double[][] photo = (double[][]) allData.elementAt(3);
		      for (int j = 0; j < height; j++) {
			      for (int ij = 0; ij < width; ij++) {
				      if (shift128Pixels && ij == 128)
					      buffer[index++] = (short) ((photo[ij - 1][j] + photo[ij + 1][j]) * 0.5);
				      else
				        buffer[index++] = (short) photo[ij][j];
		        }
	        }

	        FileInfo fileinfo = new FileInfo();
	        fileinfo.fileFormat = 1;
	        fileinfo.fileName = string_2_;
	        fileinfo.directory = string_1_;
	        fileinfo.width = width;
	        fileinfo.height = height;
	        fileinfo.offset = 0;
	        fileinfo.nImages = 1; // nImages;  todo, try with all of them
	        fileinfo.gapBetweenImages = 0;
	        fileinfo.intelByteOrder = true;
	        fileinfo.whiteIsZero = true;
	        fileinfo.fileType = fileType;
	        fileinfo.pixelWidth = pixelWidth;
	        fileinfo.pixelHeight = pixelHeight;
	        fileinfo.pixelDepth = pixelDepth;
	        fileinfo.unit = "mm";
//	        if (IJ.debugMode)
//		        IJ.write("ImportDialog: " + fileinfo);
	        open(true, buffer, fileinfo);
        } catch (Exception e) {
          e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }
      }
    }
  }

//   * Opens the image. Displays it if 'show' is
//   * true. Returns an ImagePlus object if successful.

  public void open(boolean show, short[] buffer, FileInfo fi) {
    if (buffer == null) return;
	  ImageProcessor ip = new ShortProcessor(x_size, y_size, buffer, null);
	  ImagePlus imp = new ImagePlus(fi.fileName, ip);
    imp.setFileInfo(fi);
    setCalibration(imp, fi);
    if (fi.info != null)
      imp.setProperty("Info", fi.info);
    setProperties(imp);
    if (show) imp.show();
    IJ.showProgress(1.0);
  }
	
	public Vector readAllFieldsInSection(BufferedReader bis, String leadingLine) throws IOException {
		int tokensWidth;
		int parametersNumber;
		int parXline;
		int fieldsLines = 0;
		Vector buffer = new Vector(2, 1);
		int lineLength = 80;
		String[] fields = null;
		String[] svalues = null;
		int[] ivalues = null;
		double[] dvalues = null;
		int type = -1;

//		System.out.println("Leading-" + leadingLine);

		if (leadingLine.startsWith("AAAAAAAAAAAAAAA"))
			type = 0;
		else if (leadingLine.startsWith("IIIIIIIIIIIIIIII"))
			type = 1;
		else if (leadingLine.startsWith("FFFFFFFFFFFFFFFF"))
			type = 2;
		else if (leadingLine.startsWith("SSSSSSSSSSSSSSSS"))
			type = 3;

		String line = bis.readLine();
//		System.out.println("FirstLine-" + line);

		StringTokenizer st = new StringTokenizer(line, " \t\r\n");

		parametersNumber = Integer.parseInt(st.nextToken());
		if (st.hasMoreTokens())
			fieldsLines = Integer.parseInt(st.nextToken());

		switch (type) {
			case 0:  //   AAAAAAAAAAAAAAAAA
				tokensWidth = parametersNumber;
				parametersNumber = fieldsLines;
				svalues = new String[parametersNumber];
				break;
			case 1:  //   IIIIIIIIIIIIIIIII
				tokensWidth = 8;
				ivalues = new int[parametersNumber];
				break;
			case 2:  //   FFFFFFFFFFFFFFFF
				tokensWidth = 16;
				dvalues = new double[parametersNumber];
				break;
			case 3: //   SSSSSSSSSSSSSSSS
				if (st.hasMoreTokens()) {
					int firstValue = parametersNumber;
					parametersNumber = 6;
					tokensWidth = 8;
					fields = new String[parametersNumber];
					ivalues = new int[parametersNumber];
					ivalues[0] = firstValue;
					ivalues[1] = fieldsLines;
					for (int i = 2; i < parametersNumber; i++)
						ivalues[i] = Integer.parseInt(st.nextToken());
				}
				buffer.add(fields);
				buffer.add(ivalues);
				return buffer;
			default: { // RRRRRRRRRRRRRRRRRRRRR
				buffer.add(fields);
				buffer.add(ivalues);
				return buffer;
			}
		}
		parXline = lineLength / tokensWidth;
		fields = new String[parametersNumber];

		int ip = 0;
		for (int i = 0; i < fieldsLines; i++) {
			line = bis.readLine();
//			System.out.println("Parameters-" + line);
			for (int j = 0; j < parXline && ip < parametersNumber; j++)
				fields[ip++] = Misc.toStringStripLeadingTrailingBlankAndTab(line.substring(j * tokensWidth, (j+1)*tokensWidth));
		}
		buffer.add(fields);
		
		ip = 0;
		while (ip < parametersNumber) {
			line = bis.readLine();
//			System.out.println("Data-" + line);
//			System.out.println("Par # and width: " + parametersNumber + " " + tokensWidth);
			for (int j = 0; j < parXline && ip < parametersNumber; j++) {
				String value = line.substring(j * tokensWidth, (j+1)*tokensWidth);
				switch (type) {
					case 0:  //   AAAAAAAAAAAAAAAAA
						svalues[ip++] = value;
						break;
					case 1:  //   IIIIIIIIIIIIIIIII
//						System.out.println(value);
						ivalues[ip++] = Integer.parseInt(Misc.toStringStripLeadingTrailingBlankAndTab(value));
						break;
					case 2:  //   FFFFFFFFFFFFFFFF
						dvalues[ip++] = Double.parseDouble(Misc.toStringStripLeadingTrailingBlankAndTab(value));
						break;
					default: { // RRRRRRRRRRRRRRRRRRRRR
						ip++;
					}
				}
			}
		}
		switch (type) {
			case 0:  //   AAAAAAAAAAAAAAAAA
				buffer.add(svalues);
				break;
			case 1:  //   IIIIIIIIIIIIIIIII
				buffer.add(ivalues);
				break;
			case 2:  //   FFFFFFFFFFFFFFFF
				buffer.add(dvalues);
				break;
			default: { // RRRRRRRRRRRRRRRRRRRRR
				buffer.add(svalues);
			}
		}
		return buffer;
	}
	
	public int getIntValueForKey(Vector buffer, String key) {
		String svalues[] = (String[]) buffer.get(0);
		int ivalues[] = (int[]) buffer.get(1);
		for (int i = 0; i < svalues.length; i++)
			if (key.equalsIgnoreCase(svalues[i]))
				return ivalues[i];
		return 0;
	}

	public double getDoubleValueForKey(Vector buffer, String key) {
		String svalues[] = (String[]) buffer.get(0);
		double dvalues[] = (double[]) buffer.get(1);
		for (int i = 0; i < svalues.length; i++)
			if (key.equalsIgnoreCase(svalues[i]))
				return dvalues[i];
		return 0;
	}

	public String getValueForField(String field, Vector buffer) {
		String lineKeys = ((String[]) buffer.get(0))[0];
		String lineValues = ((String[]) buffer.get(1))[0];
		int index = lineKeys.indexOf(field);
		if (index < 0)
			return null;
		String reduced = lineValues.substring(index);
		StringTokenizer st = new StringTokenizer(reduced, " \t\r\n");
		return st.nextToken();
	}

  public Vector loadImage(String folder, String name, int[] imageDimension, Vector<double[]> dimensionVector, String[] properties) {

    double chi = 0, phi = 0, omega = 0, twoTheta = 0, wavelength = 0, sample_detector_distance = 0;
    double[] x_coord = null, y_coord = null, z_coord = null;
    Vector buffer = null, finalBuffer = null;
	  String title = null;
	  double x_start = 0.0;
	  double y_start = 0.0;
	  double x_pixel = MaudPreferences.getDouble("d19Detector.pixelX", 2.5);
	  double y_pixel = MaudPreferences.getDouble("d19Detector.pixelY", 1.5);
	  boolean shift128Pixels = MaudPreferences.getBoolean("d19Detector.shift128Pixel", true);
	  double centerX = MaudPreferences.getDouble("d19Detector.ImageCenterX", -58);
	  double centerY = MaudPreferences.getDouble("d19Detector.ImageCenterY", 194.11);
	  double radius = MaudPreferences.getDouble("d19Detector.detectorSampleDistance", 768.8);

	  double coneInterval = MaudPreferences.getDouble("d19Detector.defaultEtaConeInterval", 5.0);
	  double theta2Step = MaudPreferences.getDouble("d19Detector.defaultDiffractionStepAngle", 0.05);
	  double coneAngleMax = MaudPreferences.getDouble("d19Detector.defaultEtaConeAngleMax", 180.0);

    try {
      BufferedReader bis = Misc.getReader(folder, name);

      String line = bis.readLine();

	    while (!line.startsWith("AAAAAAAAAAAAAAAAAA")) {
//	      buffer = readAllFieldsInSection(bis, line);  // we read titles and comments
		    line = bis.readLine();
//		    System.out.println("1-" + line);
	    }
	    buffer = readAllFieldsInSection(bis, line); // parameters
		  String instrument = getValueForField("Inst", buffer);
	    if (!instrument.equalsIgnoreCase("D19")) {
		    System.out.println("This is not data from D19!! Instrument = " + instrument);
//		    return null;
	    }
	    line = bis.readLine();
	    while (!line.startsWith("AAAAAAAAAAAAAAAAAA")) {
		    line = bis.readLine();
	    }
	    buffer = readAllFieldsInSection(bis, line); // parameters
	    String scanType =  getValueForField("Scantype", buffer);

/*	    title = ((String[]) buffer.get(1))[0];
	    String scanType = "phi";
	    if (title != null) {
//		    System.out.println(title);
		    scanType = Misc.toStringStripLeadingTrailingBlankAndTab(title.substring(72, 80));
	    }*/

	    line = bis.readLine();
	    while (!line.startsWith("IIIIIIIIIIIIIIII")) {
		    line = bis.readLine();
	    }
	    buffer = readAllFieldsInSection(bis, line); // parameters
	    z_size = getIntValueForKey(buffer, "npdone");
	    int numberOfDataPerImages = getIntValueForKey(buffer, "nbdata");
	    line = bis.readLine();
	    while (!line.startsWith("FFFFFFFFFFFFFFF")) {
		    line = bis.readLine();
	    }
	    buffer = readAllFieldsInSection(bis, line); // parameters
	    chi = getDoubleValueForKey(buffer, "chi") - 90;
	    omega = getDoubleValueForKey(buffer, "omega");
	    phi = getDoubleValueForKey(buffer, "phi");
	    twoTheta = getDoubleValueForKey(buffer, "2theta");
	    wavelength = getDoubleValueForKey(buffer, "wavelength");
	    sample_detector_distance = getDoubleValueForKey(buffer, "Ddetector");


	    double z_start = getDoubleValueForKey(buffer, "scan start");
	    double z_step = getDoubleValueForKey(buffer, "scan step");
	    /*double z_width = getDoubleValueForKey(buffer, "scan width");
	    z_size = (int) ((z_width + 0.000001) / z_step) + 1;*/

	    x_coord = new double[x_size];
	    y_coord = new double[y_size];
	    z_coord = new double[z_size];

	    for (int i = 0; i < x_size; i++)
		    x_coord[i] = x_start + x_pixel * i;
	    for (int i = 0; i < y_size; i++)
		    y_coord[i] = y_start + y_pixel * i;
	    if (MaudPreferences.getBoolean("d19Detector.invertYcoord", true)) {
		    double yend = y_coord[y_size - 1];
		    for (int i = 0; i < y_size; i++)
			    y_coord[i] = yend - y_coord[i];
	    }
	    for (int i = 0; i < z_size; i++)
	      z_coord[i] = z_start + z_step * i;

	    finalBuffer = new Vector(z_size + 3);
	    finalBuffer.addElement(x_coord);
	    finalBuffer.addElement(y_coord);
	    finalBuffer.addElement(z_coord);

	    for (int i = 0; i < z_size; i++) {
	      buffer = readAllFieldsInSection(bis, line = bis.readLine());
	      buffer = readAllFieldsInSection(bis, line = bis.readLine());
		    if (scanType.equalsIgnoreCase("phi"))
			    phi = getDoubleValueForKey(buffer, "angles*1000") / 1000;
		    else if (scanType.equalsIgnoreCase("chi"))
			    chi = getDoubleValueForKey(buffer, "angles*1000") / 1000;
		    else if (scanType.equalsIgnoreCase("omega"))
		      omega = getDoubleValueForKey(buffer, "angles*1000") / 1000;
		    else if (scanType.equalsIgnoreCase("2theta"))
			    twoTheta = getDoubleValueForKey(buffer, "angles*1000") / 1000;

		    double[] dimension = new double[6];
		    dimension[0] = omega;
		    dimension[1] = chi;
		    dimension[2] = phi;
		    dimension[3] = twoTheta;
		    dimension[4] = wavelength;
		    dimension[5] = sample_detector_distance;
		    dimensionVector.add(dimension);

		    buffer = readAllFieldsInSection(bis, line = bis.readLine()); // data
		    int[] intensities = (int[]) buffer.get(1);

		    int is = 0;
		    double[][] intensity = new double[x_size][y_size];
				for (int m = x_size - 1; m > -1; m--)
					for (int l = y_size - 1; l > -1; l--)
						intensity[m][l] = intensities[is++];
		    if (shift128Pixels) {
					for (int j = 0; j < y_size; j++) {
//						intensity[128][j] = intensity[127][j];
//						intensity[127][j] = intensity[126][j];
//						intensity[126][j] = intensity[125][j];
						intensity[128][j] = (int) ((intensity[127][j] + intensity[129][j]) * 0.5);
				  }
		    }

		    finalBuffer.addElement(intensity);

	    }
    } catch (IOException e) {
	    e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
    }
	    imageDimension[0] = x_size;
	    imageDimension[1] = y_size;
	    imageDimension[2] = z_size;

	    properties[0] = "D19 data";

    return finalBuffer;
  }

  void setProperties(ImagePlus imp) {
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


//   * Returns an IndexColorModel for the image specified by this FileInfo.

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
        ("About D19 Image Reader...",
            "This plugin reads image with the \"_LAMP\" format from ILL.");
  }

	class StringStringCouple {
		public String field;
		public String value;

		public StringStringCouple(String fieldS, String valueS) {
			field = fieldS;
			value = valueS;
		}
	}

	class StringIntCouple {
		public String field;
		public int value;

		public StringIntCouple(String fieldS, int valueS) {
			field = fieldS;
			value = valueS;
		}
	}

	class StringDoubleCouple {
		public String field;
		public double value;

		public StringDoubleCouple(String fieldS, double valueS) {
			field = fieldS;
			value = valueS;
		}
	}

}
