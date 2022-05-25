/*
**************************************************************************
**
**    Class  ThermalColorMap
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

package it.unitn.ing.jgraph;

import java.awt.*;
//import java.awt.image.*;
//import java.beans.PropertyChangeListener;
//import java.beans.PropertyChangeSupport;
//import java.beans.PropertyChangeEvent;

/**
 * <code>ThermalColorMap</code> provides a mapping from a value to a
 * <code>Color</code>.
 *
 * @version  $Revision: 1.6 $, $Date: 2006/01/19 14:45:51 $.
 * @author   Luca Lutterotti
 */

public class ThermalColorMap {
  protected Color[] colors_;
  double intercept = 0.0, tangent = 1.0;
  public static int[] red64 =
          {0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0,
           0, 7, 23, 39, 55, 71, 87, 103,
           119, 135, 151, 167, 183, 199, 215, 231,
           247, 255, 255, 255, 255, 255, 255, 255,
           255, 255, 255, 255, 255, 255, 255, 255,
           255, 246, 228, 211, 193, 175, 158, 140};
  public static int[] green64 =
          {0, 0, 0, 0, 0, 0, 0, 0,
           0, 11, 27, 43, 59, 75, 91, 107,
           123, 139, 155, 171, 187, 203, 219, 235,
           251, 255, 255, 255, 255, 255, 255, 255,
           255, 255, 255, 255, 255, 255, 255, 255,
           255, 247, 231, 215, 199, 183, 167, 151,
           135, 119, 103, 87, 71, 55, 39, 23,
           7, 0, 0, 0, 0, 0, 0, 0};
  public static int[] blue64 =
          {0, 143, 159, 175, 191, 207, 223, 239,
           255, 255, 255, 255, 255, 255, 255, 255,
           255, 255, 255, 255, 255, 255, 255, 255,
           255, 247, 231, 215, 199, 183, 167, 151,
           135, 119, 103, 87, 71, 55, 39, 23,
           7, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0,
           0, 0, 0, 0, 0, 0, 0, 0};

  public static int[] red16 =
          {0, 0, 0, 0, 0, 0, 51, 102, 153, 204, 255, 255, 255, 255, 204, 153};
  public static int[] green16 =
          {0, 0, 51, 102, 153, 204, 255, 255, 255, 255, 255, 204, 153, 102, 51, 0};
  public static int[] blue16 =
          {0, 153, 204, 255, 255, 255, 204, 153, 102, 51, 0, 0, 0, 0, 0, 0};

  /**
   * Initialize the color map with default thermal color map.
   * Sets up <code>ThermalColorMap</code>
   * for <code>INDEXED</code> access.
   *
   * @see java.awt.Color
   */
  public ThermalColorMap(double min, double max, int colrsNumber, boolean grayScale) {
    this(colrsNumber, grayScale);
    setRange(min, max);
  }

  /**
   * Initialize the color map with default thermal color map.
   * Sets up <code>ThermalColorMap</code>
   * for <code>INDEXED</code> access.
   *
   * @see java.awt.Color
   */
  public ThermalColorMap(int colrsNumber, boolean grayScale) {
    int[] red, green, blue;

    int numberOfColors = colrsNumber;
    red = new int[numberOfColors];
    green = new int[numberOfColors];
    blue = new int[numberOfColors];

    if (grayScale) {
      double fract = 248.0 / (colrsNumber - 1);
      for (int i = 0; i < colrsNumber; i++) {
        int grayLevel = (int) (fract * (colrsNumber - i - 1));
        red[i] = grayLevel;
        green[i] = grayLevel;
        blue[i] = grayLevel;
      }
    } else if (colrsNumber == 64) {
      for (int i = 0; i < 64; i++) {
        red[i] = red64[i];
        green[i] = green64[i];
        blue[i] = blue64[i];
      }
    } else if (colrsNumber == 16) {
      for (int i = 0; i < 16; i++) {
        red[i] = red16[i];
        green[i] = green16[i];
        blue[i] = blue16[i];
      }
    } else {
      // 1/3 -> 0     2/3 -> 0-255    5/6 -> 255   6/6 -> 255-(255*.6)
      // 1/3 -> 0-255     2/3 -> 255     3/3 -> 255-0
      // 1/6 -> (255*.6)-255   2/6 -> 255   2/3 -> 255-0     3/3 -> 0

      red[0] = 0;
      green[0] = 0;
      blue[0] = 0;
      for (int i = 1; i < numberOfColors; i++) {
        int[] sectorAndRem = getSectorAndRemaining(i - 1, numberOfColors - 1);
        switch (sectorAndRem[0]) {
          case 1:
            red[i] = 0;
            green[i] = 255 - sectorAndRem[2];
            blue[i] = 255 / 2 + (255 / 2 - sectorAndRem[1]);
            break;
          case 2:
            red[i] = 0;
            green[i] = 255 - sectorAndRem[2];
            blue[i] = 255;
            break;
          case 4:
            red[i] = 255 - sectorAndRem[2];
            green[i] = 255;
            blue[i] = sectorAndRem[2];
            break;
          case 5:
            red[i] = 255;
            green[i] = sectorAndRem[2];
            blue[i] = 0;
            break;
          default:
            {
              // case 6
              red[i] = 255 - (255 / 2 - sectorAndRem[1]);
              green[i] = sectorAndRem[2];
              blue[i] = 0;
            }
        }
//      System.out.println(i + " "+sectorAndRem[0]+" "+sectorAndRem[1]+" "+sectorAndRem[2]);
        if (red[i] < 0)
          red[i] = 0;
        if (red[i] > 255)
          red[i] = 255;
        if (green[i] < 0)
          green[i] = 0;
        if (green[i] > 255)
          green[i] = 255;
        if (blue[i] < 0)
          blue[i] = 0;
        if (blue[i] > 255)
          blue[i] = 255;
//        System.out.println(i+" " +red[i]+" "+green[i]+" "+blue[i]);
      }
    }


    colors_ = new Color[red.length];
    for (int indx = 0; indx < red.length; indx++) {
      colors_[indx] = new Color(red[indx], green[indx], blue[indx]);
    }
    setRange(0.0, (double) colors_.length);
  }

  public int[] getSectorAndRemaining(double i, double numberOfColors) {
    int[] sector = new int[3];
    int sect3 = 0;
    if (i < numberOfColors / 6.0) {
      sector[0] = 1;
      sect3 = 1;
    } else if (i < numberOfColors / 3.0) {
      sector[0] = 2;
      sect3 = 1;
    } else if (i < 2.0 / 3.0 * numberOfColors) {
      sector[0] = 4;
      sect3 = 2;
    } else if (i < 5.0 / 6.0 * numberOfColors) {
      sector[0] = 5;
      sect3 = 3;
    } else {
      sector[0] = 6;
      sect3 = 3;
    }
    double res = (1.0 / 6.0 * sector[0] + 0.00000001 - i / numberOfColors) * 6 / 2 * 255.0;
    sector[1] = (int) res;
    res = (1.0 / 3.0 * sect3 + 0.00000001 - i / numberOfColors) * 3 * 255.0;
    sector[2] = (int) res;
    return sector;
  }

  /**
   * Initialize the color map with int arrays of red, green, and blue.
   * The arrays must be the same length. Sets up <code>ThermalColorMap</code>
   * for <code>INDEXED</code> access.
   *
   * @param red Array of the red component 0 - 255.
   * @param green Array of the green component 0 - 255.
   * @param blue Array of the blue component 0 - 255.
   *
   * @see java.awt.Color
   */
  public ThermalColorMap(int[] red, int[] green, int[] blue) {
    int indx;
    colors_ = new Color[red.length];
    for (indx = 0; indx < red.length; indx++) {
      colors_[indx] = new Color(red[indx], green[indx], blue[indx]);
    }
    setRange(0.0, (double) colors_.length);
  }

  /**
   * Initialize the color map with float arrays of red, green, and blue.
   * The arrays must be the same length. Sets up <code>ThermalColorMap</code>
   * for <code>INDEXED</code> access.
   *
   * @param red Array of the red component 0.0 - 1.0.
   * @param green Array of the green component 0.0 - 1.0.
   * @param blue Array of the blue component 0.0 - 1.0.
   *
   * @see java.awt.Color
   */
  public ThermalColorMap(float[] red, float[] green, float[] blue) {
    int indx;
    colors_ = new Color[red.length];
    for (indx = 0; indx < red.length; indx++) {
      colors_[indx] = new Color(red[indx], green[indx], blue[indx]);
    }
    setRange(0.0, (double) colors_.length);
  }

  /**
   * Initialize the color map with an array of <code>Color</code>
   * objects. Sets up <code>ThermalColorMap</code> for
   * <code>INDEXED</code> access.
   *
   * @param colors Array of the Color objects.
   *
   * @see java.awt.Color
   */
  public ThermalColorMap(Color[] colors) {
    colors_ = colors;
    setRange(0.0, (double) colors_.length);
  }

  /**
   * Get a <code>Color</code>.
   *
   * @return color
   */
  public Color getColor(int indx) {
    return colors_[indx];
  }

  /**
   * Get a <code>Color</code>.
   *
   * @param val Value
   * @return Color
   *
   */
  public Color getColor(double val) {
    int indx = getTransformedIndex(val);
    if (indx < 0)
      indx = 0;
    if (indx > colors_.length - 1)
      indx = colors_.length - 1;
    return colors_[indx];
  }

  /**
   * Set the user range for the <code>Transform</codes>.
   *
   */
  public void setRange(double min, double max) {

    // min -> 0
    // max -> colors_.length-1
    // value = min + i * (max - min) / colors_.length
    // i = (value - min) * colors_.length / (max - min);

    intercept = -min * colors_.length / (max - min);
    tangent = colors_.length / (max - min);

  }

  /**
   * Get the current user range for the <code>Transform</code>.
   *
   * @return user range
   */
  public int getTransformedIndex(double value) {
    return (int) (intercept + value * tangent);
  }

  /**
   * Change the <code>Color</code>.
   *
   * @param colr new <code>Color</code>
   * @param index index of color
   */
  public void setColor(int index, Color colr) {
    setColor(index, colr.getRed(), colr.getGreen(), colr.getBlue());
  }

  /**
   * Change the <code>Color</code>.
   *
   * @param red red component
   * @param green green component
   * @param blue blue component
   * @param indx index of color
   */
  public void setColor(int indx, int red, int green, int blue) {
    if (indx < 0 || indx > colors_.length) return;
    Color newColor = new Color(red, green, blue);
    if (!colors_[indx].equals(newColor)) {
//      Color tempOld = colors_[indx];
      colors_[indx] = newColor;
    }
  }

  /**
   * Get the maximum color index.
   *
   * @return maximum legal color index
   */
  public int getMaximumIndex() {
    return colors_.length - 1;
  }

  public boolean equals(Object cm) {
    if (cm == null || !(cm instanceof ThermalColorMap)) return false;
    if (colors_.length != ((ThermalColorMap) cm).colors_.length) return false;
    for (int i = 0; i < colors_.length; i++) {
      if (!colors_[i].equals(((ThermalColorMap) cm).colors_[i])) return false;
    }
    return true;
  }
}
