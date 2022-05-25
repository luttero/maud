package it.unitn.ing.jgraph;

import java.awt.*;
import java.lang.*;


/*
**************************************************************************
**
**    Class  VectorSet
**
**************************************************************************
**    Copyright (C) 1996 Leigh Brookshaw
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
**    This class extends the DataSet class to vectors
**
*************************************************************************/


/**
 *  This class is designed to hold vectors to be plotted. It extends the
 *  DataSet class.
 *  The vectors are defined as (x,y,dx,dy) where (x,y) is the position
 *  of the vector tail and (dx,dy) is the relative position of the head.
 *  It is to be used in conjunction with the Graph2D class and Axis
 *  class for plotting 2D graphs.
 *
 * @version $Revision: 1.3 $, $Date: 2006/01/19 14:45:51 $
 * @author Leigh Brookshaw
 */
public class VectorSet extends DataSet {

  /**
   * The Default stride of the Vector class
   */
  private final static int VECTOR_STRIDE = 4;

  /**
   * Boolean set if legend is to be drawn
   */
  private boolean drawlegend = false;

/*
***************************
** Public Static Values
**************************/
  /**
   * A constant value flag used to specify if the mean magnitude of the
   * vectors is going to be used as the scaling variable
   */
  public final static int MEAN = 1;

  /**
   * A constant value flag used to specify if the minimum magnitude of the
   * vectors is going to be used as the scaling variable
   */
  public final static int MINIMUM = 2;

  /**
   * A constant value flag used to specify if the max magnitude of the
   * vectors is going to be used as the scaling variable
   */
  public final static int MAXIMUM = 3;
/*
***********************
** Public Variables
**********************/

  /**
   * This is the scaling to be used when drawing vectors. The scaling is the
   * fraction of the axis the mean vector magnitude will be scaled to.
   */
  public double scale = 0.1;


/*
*********************
** Protected Variables
**********************/
  /**
   * This is the stride of the data in the data array. For a vector set
   * it will be 4.
   */
  protected int stride = VECTOR_STRIDE;
  /**
   * The flag specifying which scaling variable to use
   */
  protected int scalingType = 1;
/*
*********************
** Private Variables
*********************/

  /**
   * The mean magnitude squared of the vectors
   */
  private double vmean;

  /**
   * The minimum magnitude squared of the vectors
   */
  private double vmin;

  /**
   * The maximum magnitude squared of the vectors
   */
  private double vmax;




/*
*********************
** Constructors
********************/

  /**
   *  Instantiate an empty data set.
   * @exception  Exception
   *            A Generic exception if it fails to instantiate the
   *            the class with the correct stride.
   */
  public VectorSet() throws Exception {
    super(VECTOR_STRIDE);
    stride = VECTOR_STRIDE;
  };

  /**
   * Instantiate a DataSet with the parsed data.
   * The double array contains the data.
   * The data is stored in the array in the sequence
   * <PRE>
   *             x,y,dx,dy,x,y,dx,dy,...
   * </PRE>
   * Where (x,y) is the position of the tail and (dx,dy) is the relative
   * position of the head.
   * This means that the length of the data
   * array is 4*n.
   * @param d Array containing the (x,y,dy,dx) vectors.
   * @param n Number of (x,y) data pairs in the array.
   * @exception  Exception
   *            A Generic exception if it fails to load the
   *            parsed array into the class.
   */
  public VectorSet(double d[], int n) throws Exception {
    super(d, n, VECTOR_STRIDE);
    this.stride = VECTOR_STRIDE;
  }

  /**
   *  Instantiate an empty data set.
   * @param scale The scaling to use when plotting the vectors.
   * @exception  Exception
   *            A Generic exception if it fails to instantiate the
   *            the class with the correct stride.
   */
  public VectorSet(double scale) throws Exception {
    super(VECTOR_STRIDE);
    this.scale = scale;
  }

  /**
   * Instantiate a DataSet with the parsed data.
   * @param d Array containing the (x,y,dy,dx) vectors.
   * @param n Number of (x,y,dx,dy) vectors in the array.
   * @exception  Exception
   *            A Generic exception if it fails to load the
   *            parsed array into the class.
   */

  public VectorSet(double d[], int n, double scale) throws Exception {
    this(d, n);
    this.scale = scale;
  }

/*
*******************
** Public Methods
******************/

  /**
   * Set the scaling to use when drawing vectors
   * @param scale The scaling to employ
   */
  public void setScale(double scale) {
    this.scale = scale;
  }

  /**
   * Set the scaling type to use when drawing vectors
   * @param type Either MEAN, MAXIMUM or MINIMUM.
   */
  public void setScalingType(int type) {
    if (type == MEAN || type == MAXIMUM || type == MINIMUM)
      scalingType = type;
  }

  /**
   *  return the current scaling factor. That is the calculated scaling
   *  using the axis range the mean/max/min magnitude and the percentage
   *  scale.
   */
  public double getScaleFactor() {
    double f;


    if (xrange > yrange)
      f = scale * yrange;
    else
      f = scale * xrange;

    if (vmean <= 0.0) return 1.0;

    if (scalingType == MEAN) return f / vmean;
    if (scalingType == MINIMUM) return f / vmin;
    if (scalingType == MAXIMUM) return f / vmax;


    return 1.0;
  }

  /**
   * Draw a Vector legend in the graph window. The legend will be placed
   * above the data window in the center
   */
  public void legend() {
    super.legend(-1, -1, null);
    drawlegend = true;
  }

  /**
   * Define a Vector legend in the graph window. The legend will be placed
   * above the data window in the center
   * @param text text to display in the legend
   */
  public void legend(String text) {
    super.legend(-1, -1, text);
    drawlegend = true;
  }

  /**
   * Define a Vector legend in the graph window
   * @param x    pixel position of the legend.
   * @param y    pixel position of the legend.
   * @param text text to display in the legend
   */
  public void legend(int x, int y, String text) {
    super.legend(x, y, text);
    drawlegend = true;
  }

  /**
   * Define a Vector legend in the graph window
   * @param x    data position of the legend.
   * @param y    data position of the legend.
   * @param text text to display in the legend
   */
  public void legend(double x, double y, String text) {
    super.legend(x, y, text);
    drawlegend = true;
  }


  /**
   * Draw the vectors at the data points.
   * If this data has been attached to an Axis then scale the data
   * based on the axis maximum/minimum otherwise scale using
   * the data's maximum/minimum
   * @param g Graphics state
   * @param bounds The data window to draw into
   */
  public void draw_data(Graphics g, Rectangle bounds) {
    Color c;

    if (xaxis != null) {
      xmax = xaxis.maximum;
      xmin = xaxis.minimum;
    }

    if (yaxis != null) {
      ymax = yaxis.maximum;
      ymin = yaxis.minimum;
    }


    xrange = xmax - xmin;
    yrange = ymax - ymin;

    /*
    ** draw the legend before we clip the data window
    */
    draw_legend(g, bounds);
    /*
    ** Clip the data window
    */
    if (clipping)
      g.clipRect(bounds.x, bounds.y,
              bounds.width, bounds.height);

    c = g.getColor();

    if (linecolor != null)
      g.setColor(linecolor);
    else
      g.setColor(c);


    drawVectors(g, bounds);


    g.setColor(c);
  }

  /**
   * Draw a legend for this Vector set
   * @param g Graphics context
   * @param w Data Window
   */

  protected void draw_legend(Graphics g, Rectangle w) {
    Color c = g.getColor();
    TextLine value = new TextLine();
    double dx;
    int ix,iy;
    int length;


    if (!drawlegend) return;
    /*
    ** Calculate the vector magnitude of a line legend_length
** pixels long. This will be our standard vector
    */

    dx = xrange * ((double) legend_length) / ((double) w.width)
            / getScaleFactor();

    value.parseDouble(dx, 3);
    /*
    ** Calculate the length of the legend
    */
    length = legend_length + value.getWidth(g) + value.charWidth(g, ' ');
    /*
    ** Calculate the position of the legend if needed.
    */

    if (legend_ix == 0 && legend_iy == 0) {
      legend_ix = (int) (w.x + ((legend_dx - xmin) / xrange) * w.width);
      legend_iy = (int) (w.y + (1.0 - (legend_dy - ymin) / yrange) * w.height);
    } else if (legend_ix == -1 && legend_iy == -1) {
      legend_ix = w.x + w.width / 2 - length / 2;
      legend_iy = w.y - value.getAscent(g) / 2;
    }

    /*
    ** In what follows the vector tail is the zero point. It is on
    ** the right - the vector points to the left
    */
    if (linecolor != null) g.setColor(linecolor);
    /*
    ** Draw the standard vector
    */

    g.drawLine(legend_ix, legend_iy, legend_ix + legend_length, legend_iy);

    ix = legend_ix + (int) (0.25 * (double) legend_length + 0.5);
    iy = legend_iy - (int) (0.25 * (double) legend_length + 0.5);

    g.drawLine(legend_ix, legend_iy, ix, iy);

    ix = legend_ix + (int) (0.25 * (double) legend_length + 0.5);
    iy = legend_iy + (int) (0.25 * (double) legend_length + 0.5);

    g.drawLine(legend_ix, legend_iy, ix, iy);
    /*
    ** Add the value of the standard vector. To the right of the vector
    */
    value.draw(g, legend_ix + legend_length + value.charWidth(g, ' '),
            iy, TextLine.LEFT);
    /*
    ** Add any legend text (above the vector) that might have been
    ** defined.
    */

    g.setColor(c);

    if (legend_text != null && !legend_text.isNull()) {

      legend_text.draw(g, legend_ix + length / 2,
              iy - value.getAscent(g)
              - legend_text.getDescent(g) - legend_text.getLeading(g)
              , TextLine.CENTER);

    }


  }

/*
*********************
** Protected Methods
*********************/


  protected void drawVectors(Graphics g, Rectangle w) {

    int ix0,iy0;
    int ix1,iy1;

    double x0,y0,x1,y1,dx,dy;

//      Is there any data to draw? Sometimes the draw command will
//      will be called before any data has been placed in the class.

    if (data == null || data.length < stride) return;

//      Lets draw the vectors

    for (int i = 0; i < length; i += stride) {

      x0 = data[i];
      y0 = data[i + 1];
      dx = data[i + 2] * getScaleFactor();
      dy = data[i + 3] * getScaleFactor();

      x1 = x0 + dx;
      y1 = y0 + dy;


      if (inside(x0, y0) || inside(x1, y1)) {

        ix0 = (int) (w.x + ((x0 - xmin) / xrange) * w.width);
        iy0 = (int) (w.y + (1.0 - (y0 - ymin) / yrange) * w.height);


        ix1 = (int) (w.x + ((x1 - xmin) / xrange) * w.width);
        iy1 = (int) (w.y + (1.0 - (y1 - ymin) / yrange) * w.height);

        g.drawLine(ix0, iy0, ix1, iy1);

        /*
  ** Now draw the head of the vector. To avoid scaling problems
        ** the head is drawn using pixel units. This would not work
        ** if we had multiple output devices.
        */

        dx = (double) (ix1 - ix0);
        dy = (double) (iy1 - iy0);


        ix0 = ix1 - (int) (0.25 * (dx - dy) + 0.5);
        iy0 = iy1 - (int) (0.25 * (dx + dy) + 0.5);

        g.drawLine(ix0, iy0, ix1, iy1);

        ix0 = ix1 - (int) (0.25 * (dx + dy) + 0.5);
        iy0 = iy1 - (int) (0.25 * (-dx + dy) + 0.5);

        g.drawLine(ix0, iy0, ix1, iy1);
      }

    }

  }


  /**
   * Calculate the range of the data and the magnitude of the vectors.
   * This modifies dxmin,dxmax,dymin,dymax
   * and xmin,xmax,ymin,ymax
   */

  protected void range(int stride) {
    int i;
    double mag = 0.0;


    if (length > stride) {
      dxmax = data[0];
      dymax = data[1];
      dxmin = dxmax;
      dymin = dymax;

      mag = data[2] * data[2] + data[3] * data[3];
      vmean = Math.sqrt(mag);
      vmin = mag;
      vmax = mag;

    } else {
      dxmin = 0.0;
      dxmax = 0.0;
      dymin = 0.0;
      dymax = 0.0;

      vmean = 0.0;
      vmin = 0.0;
      vmax = 0.0;

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

      mag = data[i + 2] * data[i + 2] + data[i + 3] * data[i + 3];

      vmean += Math.sqrt(mag);
      if (vmin > mag) vmin = mag;
      if (vmax < mag) vmax = mag;

    }

    if (length > stride) {
      vmin = Math.sqrt(vmin);
      vmax = Math.sqrt(vmax);
      vmean = vmean / dataPoints();
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


