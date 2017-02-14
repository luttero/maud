package it.unitn.ing.jgraph;

import java.awt.*;
import java.awt.image.MemoryImageSource;

//import it.unitn.ing.rista.util.*;

/*
**************************************************************************
**
**    Class  MapLegend
**
**************************************************************************
**    Copyright (C) 2000 Luca Lutterotti
**
**    This program is free software; you can redistribute it and/or modify
**    it under the terms of the GNU General Public License as published by
**    the Free Software Foundation; either version 2 of the License, or
**    (at your option) any later version.
**
**    This program is distributed in the hope that it will be useful,
**    but WITHOUT ANY WARRANTY; without even the implied warranty of
**    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**    GNU General Public License for more details.
**
**    You should have received a copy of the GNU General Public License
**    along with this program; if not, write to the Free Software
*    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
**************************************************************************
**
** This class extends the interactive graphics class to incorporate
** contours.
**
*************************************************************************/

/**
 * This class extends the interactive graphics class G2Dint to incorporate
 * contouring.
 *
 * @version  $Revision: 1.6 $, $Date: 2006/01/19 14:45:51 $.
 * @author   Luca Lutterotti
 */

public class MapLegend extends GraphInterface {

  Image pole = null;
  float min = 1.0E10f;
  float max = -1.0E10f;
  String minLabel = null;
  String maxLabel = null;
  String meanLabel = "1 mrd";
  double logOr1 = 1.0;
  int colrsNumber = 16;
  boolean logScale = false;

  /**
   * Instantiate the class
   */
  public MapLegend(double[] grid, int width, int height, double scaleMin, double scaleMax,
                   boolean grayScale, boolean logScale, double logOr1, String meanLabel, int colrsNumber,
                   int decimals) {
    this();
    this.logScale = logScale;
    if (logScale) {
      min = (float) Math.pow(10, scaleMin);
      max = (float) Math.pow(10, scaleMax);
    } else {
      min = (float) scaleMin;
      max = (float) scaleMax;
    }
    min = (float) ((int)(min * decimals)) / decimals;
    max = (float) ((int)(max * decimals)) / decimals;
    minLabel = Float.toString(min);
    maxLabel = Float.toString(max);
    fontSize = height / 9;
    inset = height / 10;
    lineWidth = width / 5;
    extraWidth = height - width - 2;
    this.logOr1 = logOr1;
    this.meanLabel = meanLabel;
    this.colrsNumber = colrsNumber;

    pole = getImage(grid, width, height, scaleMin, scaleMax, grayScale);
  }

  /**
   * Instantiate the class
   */
  public MapLegend() {
  }

  public Dimension getPreferredSize() {
    return new Dimension(pole.getWidth(this) + fontSize * 4, pole.getHeight(this) + inset * 2);
  }

  int inset = 6;
  int extraWidth = 80;
  int fontSize = 8;
  int lineWidth = 5;

  public static final double log(double arg, double base) {
    return Math.log(arg) / Math.log(base);
  }

  public static final double log10(double arg) {
    return log(arg, 10.0);
  }

  public void paint(Graphics g) {
    super.paint(g);
    g.setColor(Color.white);
    g.fillRect(0, 0, getSize().width, getSize().height);
    int height = pole.getHeight(this);
    int width = pole.getWidth(this);
    g.drawImage(pole, 1, inset, this);

    int ymin = height + inset;
    int ymax = inset;
    double rmax, rmin;
    if (logScale) {
      rmax = log10(max);
      rmin = log10(min);
    } else {
      rmax = max;
      rmin = min;
    }
    int y1 = (int) ((rmax - logOr1) * height / (rmax - rmin)) + inset;
    g.setColor(Color.black);
    g.setFont(new Font("Arial", Font.PLAIN, fontSize));

    g.drawLine(width, ymin, width + lineWidth, ymin);
    g.drawString(minLabel, width + lineWidth + 2, ymin + fontSize / 2);

    g.drawLine(width, ymax, width + lineWidth, ymax);
    g.drawString(maxLabel, width + lineWidth + 2, ymax + fontSize / 2);

    if (rmax > logOr1 && rmin < logOr1) {
      g.drawLine(width, y1, width + lineWidth, y1);
      g.drawString(meanLabel, width + lineWidth + 2, y1 + fontSize / 2);
    }
  }

  public Image getImage(double[] grid, int width, int height,
                        double scaleMin, double scaleMax, boolean grayScale) {

    float min, max;

    if (scaleMin != 0 || scaleMax != 0) {
      min = (float) scaleMin;
      max = (float) scaleMax;
    } else {
      min = (float) grid[0];
      max = (float) grid[0];
      for (int j = 0; j < height; j++) {
        min = (float) Math.min(min, grid[j]);
        max = (float) Math.max(max, grid[j]);
      }
    }

    ThermalColorMap tm = new ThermalColorMap(min, max, colrsNumber, grayScale);
    int[] pixels = getPixels(grid, min, max, width, height, tm);
    return createImage(new MemoryImageSource(width, height, pixels, 0, width));
  }

  public static int[] getPixels(double[] data, double min, double max, int width,
                                int height, ThermalColorMap tm) {

    int[] pixels = new int[width * height];
    int[] i_color = null;

    int pix = 0;
    for (int j = 0; j < height; j++) {
      if (tm == null) {
        i_color = DataImageConverter.defineColorByIntensity((float) data[height - j - 1],
                (float) min, (float) max, true);
        pix = DataImageConverter.getRGB(i_color[1], i_color[0], i_color[2]);
      } else {
        Color tmcolor = tm.getColor(data[height - j - 1]);
        pix = DataImageConverter.getRGB(tmcolor.getRed(), tmcolor.getGreen(), tmcolor.getBlue());
      }
      for (int i = 0; i < width; i++)
        pixels[j * width + i] = pix;
    }

    return pixels;

  }

}

