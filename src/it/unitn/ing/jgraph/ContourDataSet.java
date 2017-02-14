package it.unitn.ing.jgraph;

import java.awt.*;
import java.lang.*;


/*
**************************************************************************
**
**    Class  ContourDataSet
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
 *  This class is designed to hold the contour data to be plotted.
 *  It is to be used in conjunction with the Graph2D class and Axis
 *  class for plotting 2D graphs.
 *
 * @version $Revision: 1.3 $, $Date: 2006/01/19 14:45:51 $
 * @author Luca Luterotti
 */
public class ContourDataSet extends DataSet {

/*
*********************
** Constructors
********************/

  /**
   *  Instantiate an empty data set.
   */
  public ContourDataSet() {
    length = 0;
    range(stride);
  }

  /**
   *  Instantiate an empty data set.
   *  @param stride the stride of the data set. The default stride is 4.
   */
  public ContourDataSet(int stride) throws Exception {
    if (stride < 2)
      throw
              new Exception("Invalid stride parameter!");
    this.stride = stride;
    length = 0;
    range(stride);
  }

  /**
   * Instantiate a DataSet with the parsed data. Default stride is 4.
   * The double array contains the data. The X data is expected in
   * the even indices, the y data in the odd. The integer n is the
   * number of data Points. This means that the length of the data
   * array is 2*n.
   * @param d Array containing the (x,y) data pairs.
   * @param n Number of (x,y) data pairs in the array.
   * @exception  Exception
   *            A Generic exception if it fails to load the
   *            parsed array into the class.
   */
  public ContourDataSet(double d[], int n) throws Exception {
    int i;
    int k = 0;

    length = 0;

    if (d == null || d.length == 0 || n <= 0) {
      throw new Exception("DataSet: Error in parsed data!");
    }

//     Copy the data locally.

    data = new double[n * stride];
    length = n * stride;

    System.arraycopy(d, 0, data, 0, length);


//     Calculate the data range.

    range(stride);


  }

  /**
   * Instantiate a DataSet with the parsed data.
   * The double array contains the data. The X data is expected to be in
   * indices i*stride where i=0,1,... The Y data is expected to be found
   * in indices i*stride+1 where i=0,1,2...
   * The integer n is the
   * number of data Points. This means that the length of the data
   * array is 2*stride.
   * @param d Array containing the (x,y) data pairs.
   * @param n Number of (x,y) data pairs in the array.
   * @param s The stride of the data.
   * @exception  Exception
   *            A Generic exception if it fails to load the
   *            parsed array into the class.
   */
  public ContourDataSet(double d[], int n, int s) throws Exception {
    if (s < 2)
      throw
              new Exception("Invalid stride parameter!");
    int i;
    int k = 0;

    length = 0;

    if (d == null || d.length == 0 || n <= 0) {
      throw new Exception("DataSet: Error in parsed data!");
    }

    this.stride = s;


//     Copy the data locally.

    data = new double[n * stride];
    length = n * stride;

    System.arraycopy(d, 0, data, 0, length);


//     Calculate the data range.

    range(stride);
  }

/*
*********************
** Protected Methods
*********************/

  /**
   * Draw into the data window the straight line segments joining the
   * data points.
   * @param g Graphics context
   * @param w Data window
   */

  protected void draw_lines(Graphics g, Rectangle w) {
    int i;
    int j;
    boolean inside0 = false;
    boolean inside1 = false;
    double x,y;
    int x0 = 0 , y0 = 0;
    int x1 = 0 , y1 = 0;
//     Calculate the clipping rectangle
    Rectangle clip = g.getClipRect();
    int xcmin = clip.x;
    int xcmax = clip.x + clip.width;
    int ycmin = clip.y;
    int ycmax = clip.y + clip.height;


//    Is there any data to draw? Sometimes the draw command will
//    be called before any data has been placed in the class.
    if (data == null || data.length < 4) return;


//          System.out.println("Drawing Data Lines!");



    for (i = 0; i < length; i += 4) {

//    Is the first point inside the drawing region ?

      inside0 = inside(data[i], data[i + 1]);

      if (inside0) {

        x0 = (int) (w.x + ((data[i] - xmin) / xrange) * w.width);
        y0 = (int) (w.y + (1.0 - (data[i + 1] - ymin) / yrange) * w.height);

        if (x0 < xcmin || x0 > xcmax ||
                y0 < ycmin || y0 > ycmax)
          inside0 = false;

      }

//        Is the second point inside the drawing region?

      inside1 = inside(data[i + 2], data[i + 3]);

//        If one point is inside the drawing region calculate the second point
      if (inside1 || inside0) {

        x1 = (int) (w.x + ((data[i + 2] - xmin) / xrange) * w.width);
        y1 = (int) (w.y + (1.0 - (data[i + 3] - ymin) / yrange) * w.height);

        g.drawLine(x0, y0, x1, y1);
      }
    }

  }

}



