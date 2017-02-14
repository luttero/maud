package it.unitn.ing.jgraph;

import java.awt.*;
import java.lang.*;


/*
**************************************************************************
**
**    Class  PeakSet
**
**************************************************************************
**    Copyright (C) 1999 Luca Lutterotti
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
**    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
**************************************************************************
**
**    This class is designed to be used in conjunction with
**    the Graph2D class and Axis class for plotting 2D graphs.
**
*************************************************************************/


/**
 *  This class is designed to hold the data to be plotted.
 *  It is to be used in conjunction with the Graph2D class and Axis
 *  class for plotting 2D graphs.
 *
 * @version $Revision: 1.3 $, $Date: 2006/01/19 14:45:51 $
 * @author Luca Lutterotti
 */
public class PeakSet extends DataSet {

  public PeakSet(double d[], int n) throws Exception {
    super(d, n);
  }

  /**
   * Define a data legend in the graph window
   * @param x    position of the legend.
   * @param y    data position of the legend.
   * @param text text to display in the legend
   */
  public void legend(int x, double y, String text) {
    if (text == null) {
      legend_text = null;
      return;
    }
    if (legend_text == null)
      legend_text = new TextLine(text);
    else
      legend_text.setText(text);
    legend_text.setJustification(TextLine.LEFT);
    legend_dx = 0.0;
    legend_dy = y;
    legend_ix = x;
    legend_iy = 0;
  }

  /**
   * Draw a legend for this data set
   * @param g Graphics context
   * @param w Data Window
   */

  protected void draw_legend(Graphics g, Rectangle w) {
    Color c = g.getColor();

    if (legend_text == null) return;
    if (legend_text.isNull()) return;

    if (legend_dx != 0.0 || legend_ix == 0)
      legend_ix = (int) (w.x + ((legend_dx - xmin) / xrange) * w.width);

    if (legend_dy != 0.0 || legend_iy == 0)
      legend_iy = (int) (w.y + (1.0 - (legend_dy - ymin) / yrange) * w.height);

    legend_text.draw(g,
            legend_ix,
            legend_iy + legend_text.getAscent(g) / 3);

    g.setColor(c);

  }

  /**
   * Return the data point that is closest to the parsed (x,y) position
   * @param x
   * @param y (x,y) position in data space.
   * @return array containing the closest data point.
   */
  public double[] getClosestPoint(double x, double y) {
    double point[] = {0.0, 0.0, 0.0};
    int i;
    double xdiff, ydiff, dist2;

    xdiff = data[0] - x;
    ydiff = 0.0;

    point[0] = data[0];
    point[1] = data[1];
    point[2] = xdiff * xdiff;


    for (i = stride; i < length - 1; i += stride) {

      xdiff = data[i] - x;
      ydiff = 0.0;


      dist2 = xdiff * xdiff;

      if (dist2 < point[2]) {
        point[0] = data[i];
        point[1] = data[i + 1];
        point[2] = dist2;
      }

    }

//           System.out.println("DataSet: closestpoint "+point[0]+", "+point[1]+", "+point[2]);

    return point;

  }

}
