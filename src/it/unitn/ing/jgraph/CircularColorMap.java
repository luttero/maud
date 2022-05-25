/*
 * @(#)CircularColorMap.java created Apr 15, 2003 Berkeley
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

package it.unitn.ing.jgraph;

import java.awt.*;

/**
 * The CircularColorMap is a class
 *  
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class CircularColorMap extends ColorMap {
  public CircularColorMap(double[][] grid, int nslices, double scaleMin, double scaleMax,
                  boolean grayScale, String label, int colrsNumber) {
    super(grid, nslices, scaleMin, scaleMax,
                  grayScale, label, colrsNumber);
  }

  public int[] getPixels(double[][] data, double min, double max, int width,
                                int height, ThermalColorMap tm) {
    int[] pixels = new int[width * height];
    int[] i_color = null;
    int pix = 0;
    double hwidth = 0.5 * width;
    double hheight = 0.5 * height;
    for (int i = 0; i < width; i++) {
      for (int j = 0; j < height; j++) {
//        double iw = i + 0.5 - hwidth;
//        double jh = j + 0.5 - hheight;
        if (!Double.isNaN(data[i][height - j - 1])) {
          if (tm == null) {
            i_color = DataImageConverter.defineColorByIntensity((float) data[i][height - j - 1],
                    (float) min, (float) max, true);
            pix = DataImageConverter.getRGB(i_color[1], i_color[0], i_color[2]);
          } else {
            Color tmcolor = tm.getColor(data[i][height - j - 1]);
            pix = DataImageConverter.getRGB(tmcolor.getRed(), tmcolor.getGreen(), tmcolor.getBlue());
          }
        } else
          pix = DataImageConverter.getRGB(255,255,255);
        pixels[j * width + i] = pix;
      }
    }
    return pixels;
  }
}
