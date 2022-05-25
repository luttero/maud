/*
 * @(#)SYNreaderNoniusKappa.java created Jun 18, 2007 Caen
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

import ij.io.FileInfo;
import ij.io.OpenDialog;
import ij.*;
import ij.measure.Calibration;
import ij.process.ImageProcessor;
import ij.process.ShortProcessor;

import java.io.*;
import java.util.Properties;
import java.awt.image.ColorModel;

import it.unitn.ing.rista.util.MaudPreferences;
import it.unitn.ing.rista.util.Angles;

/**
 * The SYNreaderNoniusKappa is a class to
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.00 $, $Date: Jun 18, 2007 11:28:51 AM $
 * @since JDK1.1
 */
public class SYNreaderNoniusKappa extends KCDReader {

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
    int shiftedColumn = MaudPreferences.getInteger("KappaCCDSynFormat.columnToShift", 454);
    for (i = 0; i < height; i++)
      for (int j = shiftedColumn; j < width; j++)
        sbuffer[i * width + (j - shiftedColumn)] = buffer[i * width + j];
    for (i = 0; i < height; i++)
      for (int j = 0; j < shiftedColumn; j++)
        sbuffer[i * width + j + width - shiftedColumn] = buffer[i * width + j];
    int shiftedRow = MaudPreferences.getInteger("KappaCCDSynFormat.rowToShift", 573);
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
    IJ.showProgress(1.0);
  }

  void showAbout() {
    IJ.showMessage
        ("About SYN Reader...",
            "This plugin reads image with the \".syn\" format from Nonius.");
  }


}
