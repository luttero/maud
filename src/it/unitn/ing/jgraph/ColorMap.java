package it.unitn.ing.jgraph;

import java.awt.*;
import java.awt.image.MemoryImageSource;
import java.util.Vector;

/*
**************************************************************************
**
**    Class  ColorMap
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
 * @version  $Revision: 1.8 $, $Date: 2006/01/19 14:45:51 $.
 * @author   Luca Lutterotti
 */

public class ColorMap extends GraphInterface {

  Image pole = null;
  int nslices = 0;
  String title = null;
  int colrsNumber = 16;
  public static double DUMMY_VALUE = Double.NaN;
//  Vector[] isolines = null;

//  public static boolean testing = false;

  /**
   * Instantiate the class
   */
  public ColorMap(double[][] grid, int nslices, double scaleMin, double scaleMax,
                  boolean grayScale, String label, int colrsNumber) {

    this.nslices = nslices;

    this.colrsNumber = colrsNumber;
    pole = getImage(grid, nslices, nslices, scaleMin, scaleMax, grayScale);
//    isolines = computeIsolines(grid, nslices, nslices, scaleMin, scaleMax, colrsNumber-1);
    title = label;
    fontSize = nslices / 7;
    inset = nslices / 12;
  }

  public Dimension getPreferredSize() {
    return new Dimension(nslices + inset * 2, nslices + inset * 2);
  }

  int inset = 6;
  int fontSize = 8;

  public void paint(Graphics g) {
    super.paint(g);
    g.setColor(Color.white);
    if (pole != null)
      g.drawImage(pole, inset, inset, this);

/*    g.setColor(Color.black);
    int niso = isolines.length;
    for (int i = 0; i < niso; i++) {
      int ncurve = isolines[i].size();
      for (int j = 0; j < ncurve; j++) {
        double[] data = (double[]) isolines[i].elementAt(j);
        int npoint = data.length;
        for (int k = 2; k < npoint; k+=2) {
          g.drawLine( (int) data[k-2] + inset, (int) data[k-1] + inset,
                      (int) data[k] + inset, (int) data[k+1] + inset);
        }
      }
    }*/

    g.setColor(Color.black);
    g.setFont(new Font("Arial", Font.PLAIN, fontSize));
    g.drawString(title, 1, fontSize + 1);
  }

  public Image getImage(double[][] grid, int width, int height,
                        double scaleMin, double scaleMax, boolean grayScale) {

    float min, max;

    if (scaleMin != 0 || scaleMax != 0) {
      min = (float) scaleMin;
      max = (float) scaleMax;
    } else {
      min = (float) grid[0][0];
      max = (float) grid[0][0];
      for (int i = 0; i < width; i++) {
        for (int j = 0; j < height; j++) {
          if (!Double.isNaN(grid[i][j])) {
            min = (float) Math.min(min, grid[i][j]);
            max = (float) Math.max(max, grid[i][j]);
          }
        }
      }
    }

    ThermalColorMap tm = new ThermalColorMap(min, max, colrsNumber, grayScale);
    int[] pixels = getPixels(grid, min, max, width, height, tm);
    return createImage(new MemoryImageSource(width, height, pixels, 0, width));
  }

  public int[] getPixels(double[][] data, double min, double max, int width,
                                int height, ThermalColorMap tm) {
    int[] pixels = new int[width * height];
    int[] i_color = null;
    int pix = 0;
    for (int i = 0; i < width; i++) {
      for (int j = 0; j < height; j++) {
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

  public static Vector[] computeIsolines(double[][] grid, int nx, int ny, double min, double max, int nlevels) {
    int i;
    double data[];

    double[] levels = getLevels(min, max, nlevels);

    IsoLineCalculator isocurve = new IsoLineCalculator(grid, nx, ny);
    Vector[] curves = new Vector[levels.length];

    Graph2D.out.println("Computing isoline, tot number " + levels.length);
    for (i = 0; i < levels.length; i++) {
      isocurve.setValue(levels[i]);
      curves[i] = new Vector(0, 1);
      Graph2D.out.println("Level " + i + ", at "+levels[i]);
      while ((data = isocurve.getCurve()) != null) {
        Graph2D.out.println("Adding isoline");
        curves[i].addElement(data);
      }
    }

    return curves;
  }

  public static double[] getLevels(double min, double max, int nlevels) {
    double[] levels = new double[nlevels];
    double delta = (max - min) / (nlevels + 1);
    for (int i = 0; i < nlevels; i++)
      levels[i] = min + (0.5 + i) * delta;
    return levels;
  }

}
