/*
 * @(#)FromOdeigerToTiff.java created May 4, 2021 Povo
 *
 * Copyright (c) 2021 Luca Lutterotti All Rights Reserved.
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
import ij.process.*;
import ij.gui.*;
import java.awt.*;
import ij.plugin.*;
import java.lang.*;
import java.io.*;

public class FromOdeigerToTiff implements PlugIn {

	static char cnumber[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'};

	public void run(String arg) {

		FileDialog fd = new FileDialog(new Frame(), "Choose first image file", FileDialog.LOAD);
		fd.setVisible(true);
		if (fd.getFile() != null) {
			String filename = fd.getDirectory() + fd.getFile();
//			System.out.println(filename);
			int dotLocation = filename.lastIndexOf(".");
			String filename1 = filename.substring(0, dotLocation);
			String extension = filename.substring(dotLocation, filename.length());
			int indexLocation = filename1.length() - 1;
			if (indexLocation > 0) {
				while (is0to9(filename1.charAt(indexLocation)) && indexLocation > 0) {
					indexLocation--;
				}
				if (indexLocation == filename1.length() - 1) {
					System.out.println("Converting: " + filename);
					String open_cmd = "open=[" + filename + "] image=[32-bit Signed] width=1030 height=1065 offset=5120 white little-endian";
					IJ.run("Raw...", open_cmd);
					IJ.run("Properties...", "channels=1 slices=1 frames=1 unit=mm pixel_width=0.075 pixel_height=0.075 voxel_depth=0.075");
					IJ.run("Add...", "value=-2");
					IJ.saveAs("Tiff", filename1 + ".tif");
					IJ.run("Close All", "");
				} else {
					String numberString = filename1.substring(indexLocation + 1, filename1.length());
					int firstIndex = Integer.valueOf(numberString).intValue();
					filename = filename1.substring(0, indexLocation + 1);
					String fileNameNoExt = filename + Integer.toString(firstIndex);
					String fileNameNumbered = fileNameNoExt + extension;
					File f = new File(fileNameNumbered);
					boolean fileExist = f != null && f.exists();
					while (fileExist) {
						System.out.println("Converting: " + fileNameNumbered);
						String open_cmd = "open=[" + fileNameNumbered + "] image=[32-bit Signed] width=1030 height=1065 offset=5120 white little-endian";
						IJ.run("Raw...", open_cmd);
						IJ.run("Properties...", "channels=1 slices=1 frames=1 unit=mm pixel_width=0.075 pixel_height=0.075 voxel_depth=0.075");
						IJ.run("Add...", "value=-2");
						IJ.saveAs("Tiff", fileNameNoExt + ".tif");
						IJ.run("Close All", "");
						firstIndex++;
						fileNameNumbered = filename + Integer.toString(firstIndex) + extension;
//					System.out.println(fileNameNumbered);
						f = new File(fileNameNumbered);
						fileExist = f != null && f.exists();
						dotLocation = fileNameNumbered.lastIndexOf(".");
						fileNameNoExt = fileNameNumbered.substring(0, dotLocation);
					}
				}
			}
		}
		fd.dispose();

	}

	public static boolean is0to9(char achar) {
		for (int i = 0; i <= 9; i++)
			if (achar == cnumber[i])
				return true;
		return false;
	}

}
