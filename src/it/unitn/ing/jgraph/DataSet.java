package it.unitn.ing.jgraph;

import java.awt.*;
import java.lang.*;


/*
**************************************************************************
**
**    Class  DataSet
**
**************************************************************************
**    Copyright (C) 1995, 1996 Leigh Brookshaw
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
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:51 $
 * @author Leigh Brookshaw
 */
public class DataSet extends Object {


/*
***************************
** Public Static Values
**************************/
  /**
   *    A constant value flag used to specify no straight line segment
   *    is to join the data points
   */
  public final static int NOLINE = 0;
  /**
   *    A constant value flag used to specify that a straight line segment
   *    is to join the data points.
   */
  public final static int LINE = 1;

/*      public static boolean macosx = false;

      static {
        String osName = System.getProperty("os.name");
        if (osName.toLowerCase().startsWith("mac os x"))
          macosx = true;
      }*/

/*
***********************
** Public Variables
**********************/

  /**
   *    The Graphics canvas that is driving the whole show.
   * @see it.unitn.ing.jgraph.Graph2D
   */
  public Graph2D g2d;

  /**
   *    The linestyle to employ when joining the data points with
   *    straight line segments. Currently only solid and no line
   *    are supported.
   */
  public int linestyle = LINE;
  /**
   *    The color of the straight line segments
   */
  public Color linecolor = null;
  /**
   *    The index of the marker to use at the data points.
   * @see it.unitn.ing.jgraph.Markers
   */
  public int marker = 0;
  /**
   *    The marker color
   */
  public Color markercolor = null;
  /**
   *    The scaling factor for the marker. Default value is 1.
   */
  public double markerscale = 1.0;
  /**
   *    The Axis object the X data is attached to. From the Axis object
   *    the scaling for the data can be derived.
   * @see it.unitn.ing.jgraph.Axis
   */
  public Axis xaxis;
  /**
   *    The Axis object the Y data is attached to.
   * @see it.unitn.ing.jgraph.Axis
   */
  public Axis yaxis;
  /**
   * The current plottable X maximum of the data.
   * This can be very different from
   * true data X maximum. The data is clipped when plotted.
   */
  public double xmax;
  /**
   * The current plottable X minimum of the data.
   * This can be very different from
   * true data X minimum. The data is clipped when plotted.
   */
  public double xmin;
  /**
   * The current plottable Y maximum of the data.
   * This can be very different from
   * true data Y maximum. The data is clipped when plotted.
   */
  public double ymax;
  /**
   * The current plottable Y minimum of the data.
   * This can be very different from
   * true data Y minimum. The data is clipped when plotted.
   */
  public double ymin;
  /**
   * Boolean to control clipping of the data window.
   * Default value is <em>true</em>, clip the data window.
   */
  public boolean clipping = true;


/*
*********************
** Protected Variables
**********************/

  /**
   * The data X maximum.
   * Once the data is loaded this will never change.
   */
  protected double dxmax;
  /**
   * The data X minimum.
   * Once the data is loaded this will never change.
   */
  protected double dxmin;
  /**
   * The data Y maximum.
   * Once the data is loaded this will never change.
   */
  protected double dymax;
  /**
   * The data Y minimum.
   * Once the data is loaded this will never change.
   */
  protected double dymin;

  /**
   * The array containing the actual data
   */
  protected double data[];
  /**
   * The color map containing the actual data colors
   */
  protected ThermalColorMap color_map = null;
  protected double color_data[];
  /**
   * The number of data points stored in the data array
   */
  protected int length;
  /**
   *    The X range of the clipped data
   */
  protected double xrange;
  /**
   *    The Y range of the clipped data
   */
  protected double yrange;

  /**
   *    The length of the example line in the data legend.
   */
  protected int legend_length = 20;

  /**
   *    The legend text
   */
  protected TextLine legend_text = null;
  /**
   * The X pixel position of the data legend
   */
  protected int legend_ix;
  /**
   * The Y pixel position of the data legend
   */
  protected int legend_iy;
  /**
   * The X data position of the data legend
   */
  protected double legend_dx;
  /**
   * The Y data position of the data legend
   */
  protected double legend_dy;
  /**
   *    The amount to increment the data array when the append method is being
   *    used.
   */
  protected int increment = 100;


  /**
   * The stride of the data. For data pairs (x,y) the stride is 2
   */
  protected int stride = 2;

/*
*********************
** Constructors
********************/

  /**
   *  Instantiate an empty data set.
   */
  public DataSet() {
    length = 0;
    range(stride);
  }

  /**
   *  Instantiate an empty data set.
   *  @param stride the stride of the data set. The default stride is 2.
   */
  public DataSet(int stride) throws Exception {
    if (stride < 2)
      throw
              new Exception("Invalid stride parameter!");
    this.stride = stride;
    length = 0;
    range(stride);
  }

  /**
   * Instantiate a DataSet with the parsed data. Default stride is 2.
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
  public DataSet(double d[], int n) throws Exception {
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
  public DataSet(double d[], int n, int s) throws Exception {
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
*******************
** Public Methods
******************/

  /**
   * Append data to the data set.
   * @param d Array containing (x,y) pairs to append
   * @param n Number of (x,y) data pairs in the array.
   * @exception Exception
   *          A generic exception if it fails to load the
   *            parsed array into the class.
   */
  public void append(double d[], int n) throws Exception {
    int i;
    int k = 0;
    double tmp[];
    int ln = n * stride;

    if (d == null || d.length == 0 || n <= 0) {
      throw new Exception("DataSet: Error in append data!");
    }

    if (data == null) data = new double[increment];

//     Copy the data locally.


    if (ln + length < data.length) {
      System.arraycopy(d, 0, data, length, ln);
      length += ln;
    } else {
      tmp = new double[ln + length + increment];

      if (length != 0) {
        System.arraycopy(data, 0, tmp, 0, length);
      }
      System.arraycopy(d, 0, tmp, length, ln);

      length += ln;
      data = tmp;
    }

//     Calculate the data range.

    range(stride);
//     Update the range on Axis that this data is attached to
    if (xaxis != null) xaxis.resetRange();
    if (yaxis != null) yaxis.resetRange();


  }

  public void setColorMap(ThermalColorMap map) {
    color_map = map;
  }

  public void setColorData(double[] colors) {
    color_data = colors;
  }

  /**
   * Delete data from the data set (start and end are inclusive).
   * The first (x,y) pair in the data set start at index 0.
   * @param start The start (x,y) pair index.
   * @param end   The end (x,y) pair index.
   */
  public void delete(int start, int end) {
    int End = stride * end;
    int Start = stride * start;

    if (length <= 0) return;

    if (End < Start) return;
    if (Start < 0) Start = 0;
    if (End > length - stride) End = length - stride;

    if (End < length - stride) {
      System.arraycopy(data, End + stride,
              data, Start, length - End - stride);
    }

    length -= End + stride - Start;


//     Calculate the data range.

    range(stride);


  }

  /**
   * Delete all the data from the data set.
   */
  public void deleteData() {
    length = 0;
    data = null;
    range(stride);
  }

  /**
   * Draw the straight line segments and/or the markers at the
   * data points.
   * If this data has been attached to an Axis then scale the data
   * based on the axis maximum/minimum otherwise scale using
   * the data's maximum/minimum
   * @param g Graphics state
   * @param bounds The data window to draw into
   */
  public void draw_data(Graphics g, Rectangle bounds) {
    Color c;

    if (xaxis != null) {
      xmax = xaxis.getMaximum();
      xmin = xaxis.getMinimum();
    }

    if (yaxis != null) {
      ymax = yaxis.getMaximum();
      ymin = yaxis.getMinimum();
    }


    xrange = xmax - xmin;
    yrange = ymax - ymin;

    /*
    ** draw the legend before we clip the data window
    */

    if (clipping) { // && !macosx) {
      Shape actualclip = g.getClip();

      g.setClip(0, bounds.y, bounds.x, bounds.height);

      draw_legend(g, bounds);

      g.setClip(actualclip);

    } else
      draw_legend(g, bounds);


    /*
    ** Clip the data window
    */

    if (clipping)
      g.clipRect(bounds.x, bounds.y, bounds.width, bounds.height);
    c = g.getColor();

    if (linestyle != DataSet.NOLINE) {
      if (linecolor != null)
        g.setColor(linecolor);
      else
        g.setColor(c);
      draw_lines(g, bounds);
    }


    if (marker > 0) {
      if (markercolor != null)
        g.setColor(markercolor);
      else
        g.setColor(c);
      draw_markers(g, bounds);
    }


    g.setColor(c);
  }

  /**
   * return the data X maximum.
   */
  public double getXmax() {
    return dxmax;
  }

  /**
   * return the data X minimum.
   */
  public double getXmin() {
    return dxmin;
  }

  /**
   * return the data Y maximum.
   */
  public double getYmax() {
    return dymax;
  }

  /**
   * return the data Y minimum.
   */
  public double getYmin() {
    return dymin;
  }


  /**
   * Define a data legend in the graph window
   * @param x    pixel position of the legend.
   * @param y    pixel position of the legend.
   * @param text text to display in the legend
   */
  public void legend(int x, int y, String text) {
    if (text == null) {
      legend_text = null;
      return;
    }
    if (legend_text == null)
      legend_text = new TextLine(text);
    else
      legend_text.setText(text);
    legend_text.setJustification(TextLine.LEFT);
    legend_ix = x;
    legend_iy = y;
    legend_dx = 0.0;
    legend_dy = 0.0;

  }

  /**
   * Define a data legend in the graph window
   * @param x    data position of the legend.
   * @param y    data position of the legend.
   * @param text text to display in the legend
   */
  public void legend(double x, double y, String text) {
    if (text == null) {
      legend_text = null;
      return;
    }
    if (legend_text == null)
      legend_text = new TextLine(text);
    else
      legend_text.setText(text);
    legend_text.setJustification(TextLine.LEFT);
    legend_dx = x;
    legend_dy = y;
    legend_ix = 0;
    legend_iy = 0;
  }

  /**
   * Set the font to be used in the legend
   * @param f font
   */
  public void legendFont(Font f) {
    if (f == null) return;
    if (legend_text == null) legend_text = new TextLine();

    legend_text.setFont(f);
  }

  /**
   * Set the color for the legend text
   * @param c color
   */
  public void legendColor(Color c) {
    if (c == null) return;
    if (legend_text == null) legend_text = new TextLine();

    legend_text.setColor(c);
  }

  /**
   * Return the number of data points in the DataSet
   * @return number of (x,y0 points.
   */
  public int dataPoints() {
    return length / stride;
  }

  /**
   * get the data point at the parsed index. The first (x,y) pair
   * is at index 0.
   * @param index Data point index
   * @return array containing the (x,y) pair.
   */
  public double[] getPoint(int index) {
    double point[] = new double[stride];
    int i = index * stride;
    if (index < 0 || i > length - stride) return null;

    for (int j = 0; j < stride; j++) point[j] = data[i + j];

    return point;
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
    ydiff = data[1] - y;

    point[0] = data[0];
    point[1] = data[1];
    point[2] = xdiff * xdiff + ydiff * ydiff;


    for (i = stride; i < length - 1; i += stride) {

      xdiff = data[i] - x;
      ydiff = data[i + 1] - y;


      dist2 = xdiff * xdiff + ydiff * ydiff;

      if (dist2 < point[2]) {
        point[0] = data[i];
        point[1] = data[i + 1];
        point[2] = dist2;
      }

    }

    //System.out.println("DataSet: closestpoint "+point[0]+", "+point[1]+", "+point[2]);

    return point;

  }


  public double[] getClosestPoint(double x) {
    double point[] = {0.0, 0.0, 0.0};
    int i;
    double xdiff, ydiff, dist2;

    xdiff = data[0] - x;

    point[0] = data[0];
    point[1] = data[1];
    point[2] = xdiff * xdiff;


    for (i = stride; i < length - 1; i += stride) {

      xdiff = data[i] - x;


      dist2 = xdiff * xdiff;

      if (dist2 < point[2]) {
        point[0] = data[i];
        point[1] = data[i + 1];
        point[2] = dist2;
      }

    }

    //System.out.println("DataSet: closestpoint "+point[0]+", "+point[1]+", "+point[2]);

    return point;

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
    if (data == null || data.length < stride) return;


//          System.out.println("Drawing Data Lines!");


//    Is the first point inside the drawing region ?
    if ((inside0 = inside(data[0], data[1]))) {

      x0 = (int) (w.x + ((data[0] - xmin) / xrange) * w.width);
      y0 = (int) (w.y + (1.0 - (data[1] - ymin) / yrange) * w.height);

      if (x0 < xcmin || x0 > xcmax ||
              y0 < ycmin || y0 > ycmax)
        inside0 = false;

    }


    for (i = stride; i < length; i += stride) {

//        Is this point inside the drawing region?

      inside1 = inside(data[i], data[i + 1]);

//        If one point is inside the drawing region calculate the second point
      if (inside1 || inside0) {

        x1 = (int) (w.x + ((data[i] - xmin) / xrange) * w.width);
        y1 = (int) (w.y + (1.0 - (data[i + 1] - ymin) / yrange) * w.height);

        if (x1 < xcmin || x1 > xcmax ||
                y1 < ycmin || y1 > ycmax)
          inside1 = false;

      }
//        If the second point is inside calculate the first point if it
//        was outside
      if (!inside0 && inside1) {

        x0 = (int) (w.x + ((data[i - stride] - xmin) / xrange) * w.width);
        y0 = (int) (w.y + (1.0 - (data[i - stride + 1] - ymin) / yrange) * w.height);

      }
//        If either point is inside and is a valid data draw the segment

      if ((!Double.isNaN(data[i - stride + 1]) && !Double.isNaN(data[i + 1])) && (inside0 || inside1))
        g.drawLine(x0, y0, x1, y1);

/*
**        The reason for the convolution above is to avoid calculating
**        the points over and over. Now just copy the second point to the
**        first and grab the next point
*/
      inside0 = inside1;
      x0 = x1;
      y0 = y1;

    }

  }


  /**
   *  Return true if the point (x,y) is inside the allowed data range.
   */

  protected boolean inside(double x, double y) {
    if (x >= xmin && x <= xmax &&
            y >= ymin && y <= ymax)
      return true;

    return false;
  }

  /**
   *  Draw the markers.
   *  Only markers inside the specified range will be drawn. Also markers
   *  close the edge of the clipping region will be clipped.
   * @param g Graphics context
   * @param w data window
   * @see it.unitn.ing.jgraph.Markers
   */
  protected void draw_markers(Graphics g, Rectangle w) {
    double x1,y1;
    int i;
//     Calculate the clipping rectangle
    Rectangle clip = g.getClipRect();
    int xcmin = clip.x;
    int xcmax = clip.x + clip.width;
    int ycmin = clip.y;
    int ycmax = clip.y + clip.height;
/*
**        Load the marker specified for this data
*/
    Markers m = g2d.getMarkers();


    if (m == null) return;

//          System.out.println("Drawing Data Markers!");

    Color c = null;
    if (color_map != null) {
      c = g.getColor();
    }

    for (i = 0; i < length; i += stride) {
      if (inside(data[i], data[i + 1])) {

        x1 = w.x + ((data[i] - xmin) / xrange) * w.width;
        y1 = w.y + (1.0 - (data[i + 1] - ymin) / yrange) * w.height;

        if (color_map != null) {
          g.setColor(color_map.getColor(color_data[i / 2]));
        }

        if (x1 >= xcmin && x1 <= xcmax &&
                y1 >= ycmin && y1 <= ycmax)
          m.draw(g, marker, markerscale, x1, y1);

      }
    }
    if (color_map != null) {
      g.setColor(c);
    }


  }

  /**
   * Draw a legend for this data set
   * @param g Graphics context
   * @param w Data Window
   */

  protected void draw_legend(Graphics g, Rectangle w) {
    Color c = g.getColor();
    Markers m = null;


    if (legend_text == null) return;
    if (legend_text.isNull()) return;

    if (legend_dx != 0.0 || legend_ix == 0)
      legend_ix = (int) (w.x + ((legend_dx - xmin) / xrange) * w.width);

    if (legend_dy != 0.0 || legend_iy == 0)
      legend_iy = (int) (w.y + (1.0 - (legend_dy - ymin) / yrange) * w.height);

    if (linestyle != DataSet.NOLINE) {
      if (linecolor != null) g.setColor(linecolor);
      g.drawLine(legend_ix, legend_iy, legend_ix + legend_length, legend_iy);
    }

    if (marker > 0) {
      m = g2d.getMarkers();
      if (m != null) {
        if (markercolor != null)
          g.setColor(markercolor);
        else
          g.setColor(c);

        m.draw(g, marker, 1.0, legend_ix + legend_length / 2, legend_iy);
      }
    }


    legend_text.draw(g,
            legend_ix + legend_length + legend_text.charWidth(g, ' '),
            legend_iy + legend_text.getAscent(g) / 3);

    g.setColor(c);

  }

  /**
   * Calculate the range of the data. This modifies dxmin,dxmax,dymin,dymax
   * and xmin,xmax,ymin,ymax
   */

  protected void range(int stride) {
    int i;


    if (length >= stride) {
      dxmax = data[0];
      dymax = data[1];
      dxmin = dxmax;
      dymin = dymax;
    } else {
      dxmin = 0.0;
      dxmax = 0.0;
      dymin = 0.0;
      dymax = 0.0;
    }

    for (i = stride; i < length; i += stride) {

      if (dxmax < data[i]) {
        dxmax = data[i];
      } else if (dxmin > data[i]) {
        dxmin = data[i];
      }

      if (dymax < data[i + 1]) {
        dymax = data[i + 1];
      } else if (dymin > data[i + 1]) {
        dymin = data[i + 1];
      }
    }

    if (xaxis == null) {
      xmin = dxmin;
      xmax = dxmax;
    }
    if (yaxis == null) {
      ymin = dymin;
      ymax = dymax;
    }
  }

}


