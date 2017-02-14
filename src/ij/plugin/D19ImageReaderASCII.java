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
 * The D19ImageReader is a class to read D19 .d19 image
 * from D19 new neutron diffractometer
 * todo: to be implemented for the imageJ part
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 16, 2008 3:57:39 PM $
 * @since JDK1.1
 */
public class D19ImageReaderASCII implements PlugIn {

/*

D19

640 x 256

764 mm radius

120 x 30 deg

2 PI r * 120 / 360 = PI / 180 * 120 * r

l = 1600.0
h = 391.5302450592896

pixel w: 2.5
pixel h: 1.52941501976285

*/

	public void run(final String string) {
		if (string.equals("about"))
			showAbout();
		else {
			OpenDialog opendialog
					= new OpenDialog("Open D19 image ASCII", null);
			String string_1_ = opendialog.getDirectory();
			String string_2_ = opendialog.getFileName();
			if (string_2_ != null) {
				try {
/*          File file = new File(string_1_ + string_2_);
          DataInputStream dis = new DataInputStream(new FileInputStream(file));
          BufferedReader bis = new BufferedReader(new InputStreamReader(dis));
          boolean endOfHeaders = false;
          do {
            endOfHeaders = decode(bis.readLine());
          } while (!endOfHeaders);
           int totalSize = (width * height);
          java.nio.ByteBuffer bb = java.nio.ByteBuffer.allocate(dis.available());
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
          open(true, buffer, fileinfo);*/
				} catch (Exception e) {
					e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
				}
			}
		}
	}

	boolean decode(String headerLine) {
/*    if (headerLine.startsWith(x_dimension)) {
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
    }*/
		return false;
	}

	/**
	 * Opens the image. Displays it if 'show' is
	 * true. Returns an ImagePlus object if successful.
	 */
	public void open(boolean show, short[] buffer, FileInfo fi) {
/*    ImagePlus imp;
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
    short[][] fbuffer = new short[height][width];
    for (i = 0; i < height; i++)
      for (int j = 0; j < width; j++)
        fbuffer[i][j] = buffer[i * width + j];
    for (i = 0; i < width; i++)
      for (int j = 0; j < height; j++)
        buffer[i * height + j] = fbuffer[j][i];
    int twidth = width;
    width = height;
    height = twidth;
    float tpixel = pixelWidth;
    pixelWidth = pixelHeight;
    pixelHeight = tpixel;
    ip = new ShortProcessor(width, height, buffer, null);
    imp = new ImagePlus(fi.fileName, ip);
    imp.setFileInfo(fi);
    setCalibration(imp, fi);
    if (fi.info != null)
      imp.setProperty("Info", fi.info);
    setProperties(imp);
    if (show) imp.show();
    IJ.showProgress(1.0);*/
	}

	public Vector loadImage(String folder, String name, int[] imageDimension, double[] dimension, String[] properties) {

		int x_size = 0, y_size = 0, z_size = 0;
		double chi = 0, phi = 0, omega = 0, twoTheta = 0, wavelenght = 0, sample_detector_distance = 0;
		String name_ascii = name + "ascii";
		double[] x_coord = null, y_coord = null, z_coord = null;
		Vector buffer = null;

		try {
			BufferedReader bis = Misc.getReader(folder, name);

			String line = bis.readLine();
			StringTokenizer st = new StringTokenizer(line, "=: ,\t\r\n");
			String token = "";
			boolean parameters = false;
			boolean titles = false;
			boolean predata = true;
			boolean xcoordinates = false;
			boolean ycoordinates = false;
			boolean zcoordinates = false;
			boolean monitors = false;

			while (line != null) {
//        System.out.println("----: " + line);
				while (st.hasMoreTokens()) {
					token = st.nextToken();
					if (token.equalsIgnoreCase("TITLES")) {
						parameters = false;
						titles = true;
						predata = false;
						xcoordinates = false;
						ycoordinates = false;
						zcoordinates = false;
						monitors = false;
					} else if (token.equalsIgnoreCase("PARAMETERS")) {
						parameters = true;
						titles = false;
						predata = false;
						xcoordinates = false;
						ycoordinates = false;
						zcoordinates = false;
						monitors = false;
					} else if (token.equalsIgnoreCase("X_COORDINATES")) {
//            System.out.println("X_COORDINATES");
						parameters = false;
						titles = false;
						predata = false;
						xcoordinates = true;
						ycoordinates = false;
						zcoordinates = false;
						monitors = false;
						line = bis.readLine();
						line = bis.readLine();
						st = new StringTokenizer(line, "=: ,\t\r\n");
					} else if (token.equalsIgnoreCase("Y_COORDINATES")) {
						//           System.out.println("Y_COORDINATES");
						parameters = false;
						titles = false;
						predata = false;
						xcoordinates = false;
						ycoordinates = true;
						zcoordinates = false;
						monitors = false;
						line = bis.readLine();
						line = bis.readLine();
						st = new StringTokenizer(line, "=: ,\t\r\n");
					} else if (token.equalsIgnoreCase("Z_COORDINATES")) {
//            System.out.println("Z_COORDINATES");
						parameters = false;
						titles = false;
						predata = false;
						xcoordinates = false;
						ycoordinates = false;
						zcoordinates = true;
						monitors = false;
						line = bis.readLine();
						line = bis.readLine();
						st = new StringTokenizer(line, "=: ,\t\r\n");
					} else if (token.equalsIgnoreCase("MONITORS")) {
						parameters = false;
						titles = false;
						predata = false;
						xcoordinates = false;
						ycoordinates = false;
						zcoordinates = false;
						monitors = true;
						line = bis.readLine();
						line = bis.readLine();
						st = new StringTokenizer(line, "=: ,\t\r\n");
					} else if (predata) {
						if (token.equalsIgnoreCase("DATA_FILE"))
							name_ascii = st.nextToken();
						else if (token.equalsIgnoreCase("X_SIZE"))
							x_size = Integer.parseInt(token = st.nextToken());
						else if (token.equalsIgnoreCase("Y_SIZE"))
							y_size = Integer.parseInt(token = st.nextToken());
						else if (token.equalsIgnoreCase("Z_SIZE"))
							z_size = Integer.parseInt(token = st.nextToken());
					} else if (parameters) {
						if (token.matches("Phi"))
							phi = Double.parseDouble(token = st.nextToken());
						else if (token.matches("Chi"))
							chi = Double.parseDouble(token = st.nextToken());
						else if (token.matches("Omega"))
							omega = Double.parseDouble(token = st.nextToken());
						else if (token.matches("2*Theta"))
							twoTheta = Double.parseDouble(token = st.nextToken());
						else if (token.matches("lenght"))
							wavelenght = Double.parseDouble(token = st.nextToken());
						else if (token.matches("Distance"))
							sample_detector_distance = Double.parseDouble(token = st.nextToken());
					}
					if (xcoordinates) {
						x_coord = new double[x_size];
//            System.out.println("X_COORDINATES " + x_size);
						int i = 0;
						while (line != null && !line.contains("--><pre>")) {
							while (st.hasMoreTokens()) {
								token = st.nextToken();
								x_coord[i++] = Double.parseDouble(token);
//                System.out.println("X " + token);
							}
							line = bis.readLine();
							if (line != null)
								st = new StringTokenizer(line, "=: ,\t\r\n");
						}
						xcoordinates = false;
					}
					if (ycoordinates) {
						y_coord = new double[y_size];
						int i = 0;
						while (line != null && !line.contains("--><pre>")) {
							while (st.hasMoreTokens()) {
								token = st.nextToken();
								y_coord[i++] = Double.parseDouble(token) * 10.0;
//                System.out.println("Y " + token);
							}
							line = bis.readLine();
							if (line != null)
								st = new StringTokenizer(line, "=: ,\t\r\n");
						}
						ycoordinates = false;
					}
					if (zcoordinates) {
						z_coord = new double[z_size];
						int i = 0;
						while (line != null && !line.contains("--><pre>")) {
							while (st.hasMoreTokens()) {
								token = st.nextToken();
								z_coord[i++] = Double.parseDouble(token);
//                System.out.println("Phi " + token);
							}
							line = bis.readLine();
							if (line != null)
								st = new StringTokenizer(line, "=: ,\t\r\n");
						}
						zcoordinates = false;
					}
				}
				line = bis.readLine();
				if (line != null)
					st = new StringTokenizer(line, "=: ,\t\r\n");
			}


			buffer = new Vector(z_size + 3);
			buffer.addElement(x_coord);
			buffer.addElement(y_coord);
			buffer.addElement(z_coord);

			BufferedReader data = Misc.getReader(folder, name_ascii);

			line = data.readLine();
			st = new StringTokenizer(line, "=: ,\t\r\n");
			token = "";

			int ix = 0, iy = 0, iz = 0;
			double[][] intensity = new double[x_size][y_size];
			while (line != null && iz < z_size) {
				while (st.hasMoreTokens()) {
					token = st.nextToken();
					intensity[ix++][iy] = Double.parseDouble(token);
					if (ix >= x_size) {
						ix = 0;
						iy++;
						if (iy >= y_size) {
							iy = 0;
							iz++;
							buffer.addElement(intensity);
							intensity = new double[x_size][y_size];
						}
					}
				}
				line = data.readLine();
				if (line != null)
					st = new StringTokenizer(line, "=: ,\t\r\n");
			}
			buffer.addElement(intensity);

		} catch (IOException e) {
			e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
		}
		imageDimension[0] = x_size;
		imageDimension[1] = y_size;
		imageDimension[2] = z_size;
		dimension[0] = omega;
		dimension[1] = chi;
		dimension[2] = phi;
		dimension[3] = twoTheta;
		dimension[4] = wavelenght;
		dimension[5] = sample_detector_distance;
		properties[0] = "D19 data";
		return buffer;
	}

	void setProperties(ImagePlus imp) {
/*    setProperty(imp, x_dimension, width);
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
    imp.setProperty(data_type, dataType);*/
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
				("About D19 ASCII Image Reader...",
						"This plugin reads image with the \"ASCII LAMP\" format from ILL.");
	}

}
