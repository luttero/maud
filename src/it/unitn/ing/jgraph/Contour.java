package it.unitn.ing.jgraph;

import java.awt.*;
import java.net.URL;
import java.util.*;
import java.io.InputStream;

/*
**************************************************************************
**
**    Class  Contour
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
 * @version  $Revision: 1.3 $, $Date: 2006/01/19 14:45:51 $.
 * @author   Leigh Brookshaw
 */

public class Contour extends G2Dint {

/*
**************
**
** Constants
**
*************/

/*
**   The minimum length of a curve before it gets a label
*/
  static final int MINCELLS = 30;
/*
**   Default number of contour levels
*/
  static final int NLEVELS = 12;


  /**********************
   **
   ** Protected Variables
   **
   ***********************/

  /**
   *   Dimension of the contour grid in the X direction
   */
  protected int nx;
  /**
   *   Dimension of the contour grid in the Y direction
   */
  protected int ny;
  /**
   *   Vector array containing the Contour curves.
   *   Each index in the array contains curves at a given
   *   contour level
   */
  protected Vector curves[];

  /**
   *   If set the class calculates the contour levels based on
   *   the data minimum and maximum. Default value <i>true</i>.
   */
  protected boolean autoLevels;
/*
 *   If true the contour levels are calculated in
 *   logarithmic intervals
 */
  protected boolean logLevels;
/*
 *   If true the limits of the plot are the limits of the
 *   data grid not the limits of the contours!
 */
  protected boolean gridLimits;
/*
 *   The array of contour levels
 */
  protected double levels[];
  /**
   *   The label for each contour level
   */
  protected TextLine labels[];
  /**
   *   Font to use in drawing Labels
   */
  protected Font labelfont;
  /**
   *   Color to use in drawing Labels
   */
  protected Color labelcolor;
  /**
   *   Style to use in drawing Labels. TextLine.SCIENTIFIC or
   *   TextLine.ALGEBRAIC.
   */
  protected int labelStyle;
  /**
   *   Precision to use in drawing Labels.
   */
  protected int labelPrecision;

  /**
   *   Number of Significant figures to use in drawing Labels.
   */
  protected int labelSignificant;

  /**
   *   Which levels will get labels. If it is equal to 1 every level
   *   gets a label, equal to 2 every second level etc. If it is equal to 0
   *   no labels are displayed.
   */
  protected int labelLevels;
  /**
   *   If false labels are not drawn
   */
  protected boolean drawlabels;
  /**
   *   If true the labels will be calculated for each
   *   contour level. These might not look all that hot.
   */
  protected boolean autoLabels;
  /**
   *  Color to draw non labelled contour line
   */
  protected Color contourColor;
  /**
   *  Color to draw labelled contour line
   */
  protected Color labelledColor;

  /**
   *   The data grid, a 2D array stored in linear form.
   *   It is assumed that [0,0] is the bottom left corner
   *   and the data is ordered by row.
   */
  protected double grid[];

  /**
   *   The X minimum limit of the data grid
   */
  protected double xmin;
  /**
   *   The X maximum limit of the data grid
   */
  protected double xmax;
  /**
   *   The Y minimum limit of the data grid
   */
  protected double ymin;
  /**
   *   The Y maximum limit of the data grid
   */
  protected double ymax;
  /**
   *   The minimum value of the grid values
   */
  protected double zmin;
  /**
   *   The maximum value of the grid values
   */
  protected double zmax;

  /**
   * Boolean value if true Contours will not be calculated
   */
  public boolean noContours = false;

/*
*****************
**
** Constructors
**
****************/

  /**
   * Instantaite the class
   */
  public Contour() {

    grid = null;
    xmin = 0.0;
    xmax = 0.0;
    ymin = 0.0;
    ymax = 0.0;
    zmin = 0.0;
    zmax = 0.0;

    nx = 0;
    ny = 0;

    levels = new double[NLEVELS];
    labels = new TextLine[NLEVELS];

    autoLevels = true;
    logLevels = false;
    gridLimits = false;
    autoLabels = true;
    labelfont = new Font("Helvetica", Font.PLAIN, 12);
    labelcolor = Color.blue;
    labelLevels = 1;
    labelStyle = TextLine.ALGEBRAIC;
    labelPrecision = 2;
    labelSignificant = 3;
    drawlabels = true;

    contourColor = null;
    labelledColor = null;

    curves = null;

  }

/*
************
**
** Methods
**
***********/
  /**
   *  Load the grid to contour from a URL. There are 2 formats for the data
   *  optionally the limits of the grid can be parsed.<BR>
   *  <PRE>
   *  The expected format of the data
   *           1st Number:   nx
   *           2nd Number:   ny
   *           nx*ny numbers following
   *
   *  Optionally
   *           1st Number:   nx
   *           2nd Number:   ny
   *           3rd Number:   xmin
   *           4th Number:   xmax
   *           5th Number:   ymin
   *           6th Number:   ymax
   *           nx*ny numbers following
   *  </PRE><BR>
   *  If xmin, xmax, ymin, ymax are not specified they are assumed
   *  to be [1.0,nx,1.0,ny]
   *
   * @param file URL of the file to load
   * @return <i>true</I> of the load was successful.
   *
   */
  public boolean loadGrid(URL file) {
    byte b[] = new byte[50];
    int nbytes = 0;
    int max = 100;
    int inc = 100;
    int n = 0;
    double data[] = new double[max];
    InputStream is = null;
    boolean comment = false;
    int c;

    try {
      is = file.openStream();

      while ((c = is.read()) > -1) {

        switch (c) {

          case '#':
            comment = true;
            break;
          case '\r':
          case '\n':
            comment = false;
          case ' ':
          case '\t':
            if (nbytes > 0) {
              String s = new String(b, 0, 0, nbytes);
              data[n] = Double.valueOf(s).doubleValue();
              n++;
              if (n >= max) {
                max += inc;
                double d[] = new double[max];
                System.arraycopy(data, 0, d, 0, n);
                data = d;
              }

              nbytes = 0;
            }
            break;
          default:
            if (!comment) {
              b[nbytes] = (byte) c;
              nbytes++;
            }
            break;
        }

      }

      if (is != null) is.close();
    } catch (Exception e) {
      Graph2D.out.println("Failed to load Grid from file ");
      e.printStackTrace();
      if (is != null)
        try {
          is.close();
        } catch (Exception ev) {
        }
      return false;
    }

    if (n < 1) {
      Graph2D.out.println("Failed to load Grid from file ");
      return false;
    }

    nx = (int) (data[0] + 0.5);
    ny = (int) (data[1] + 0.5);

    if (n == nx * ny + 6) {
      xmin = data[2];
      xmax = data[3];
      ymin = data[4];
      ymax = data[5];

      grid = new double[nx * ny];
      System.arraycopy(data, 6, grid, 0, nx * ny);
    } else if (n == nx * ny + 2) {
      xmin = 1.0;
      xmax = (double) (nx);
      ymin = 1.0;
      ymax = (double) (ny);

      grid = new double[nx * ny];
      System.arraycopy(data, 2, grid, 0, nx * ny);
    } else {
      Graph2D.out.println("Error loading grid, Wrong number of points ");
      grid = null;
      return false;
    }

    zrange();
    calcLevels();

    detachCurves();
    curves = null;

    return true;

  }

  /**
   *  Set the range of the grid
   * @param xmin Minimum X value
   * @param xmax Maximum X value
   * @param ymin Minimum Y value
   * @param ymax Maximum Y value
   */


  public void setRange(double xmin, double xmax, double ymin, double ymax) {

    if (xmin >= xmax || ymin >= ymax) return;

    this.xmin = xmin;
    this.xmax = xmax;
    this.ymin = ymin;
    this.ymax = ymax;
  }

  /**
   *  Return the range of the grid
   * @return An array contining xmin,xmax,ymin,ymax.
   */
  public double[] getRange() {
    double d[] = new double[4];
    d[0] = xmin;
    d[1] = xmax;
    d[2] = ymin;
    d[3] = ymax;

    return d;
  }

  /**
   *  return the dimensions of the grid
   * @return An array containing the number of columns, number of rows.
   */
  public int[] getDim() {
    int i[] = new int[2];

    i[0] = nx;
    i[1] = ny;

    return i;
  }

  /**
   *  Return the grid
   * @return An array of size nx by ny contining the data grid.
   */
  public double[] getGrid() {
    return grid;
  }

  /**
   * Manually set the contour levels.
   * @param levels An array containing the contour levels
   * @param nl The number of contour levels in the arrray
   */
  public void setLevels(double levels[], int nl) {
    int i;
    if (levels == null || nl <= 0) return;

    detachCurves();
    curves = null;

    autoLevels = false;

    this.levels = new double[nl];

    System.arraycopy(levels, 0, this.levels, 0, nl);

    labels = new TextLine[nl];
    for (i = 0; i < labels.length; i++) {
      labels[i] = new TextLine(String.valueOf((float) levels[i]));
    }
  }

  /**
   * Manually set the Contour labels.
   * @param labels An array containing the labels.
   * @param nl Number of labels in the Array.
   */
  public void setLabels(TextLine labels[], int nl) {
    if (labels == null || nl <= 0) return;

    autoLabels = false;
    this.labels = new TextLine[nl];

    System.arraycopy(labels, 0, this.labels, 0, nl);
  }

  /**
   * Set the font to be used with All the labels
   * @param f Font
   */
  public void setLabelFont(Font f) {
    labelfont = f;
  }

  /**
   * Set the Color to be used with all the labels.
   * @param c Color
   */
  public void setLabelColor(Color c) {
    labelcolor = c;
  }

  /**
   *  Set the grid to be contoured.
   * @param grid Array of values
   * @param nx   Number of columns
   * @param ny   Number of rows
   */
  public void setGrid(double grid[], int nx, int ny) {
    this.grid = grid;
    this.nx = nx;
    this.ny = ny;

    zrange();
    calcLevels();

  }

  /**
   * Delete all the Contours
   */
  public void deleteContours() {
    if (curves == null) return;
    detachCurves();
    curves = null;
  }

  /**
   * Detach contours so that they will not be plotted.
   */
  public void detachContours() {
    if (curves == null) return;
    detachCurves();
  }

  /**
   * Attach contours so that they will be plotted.
   */
  public void attachContours() {
    if (curves == null) return;
    attachCurves();
  }

  /**
   * Set the contour's color.
   * @param c Color
   */
  public void setContourColor(Color c) {
    contourColor = c;
  }

  /**
   * Set the labelled contour's color.
   * @param c Color
   */
  public void setLabelledContourColor(Color c) {
    labelledColor = c;
  }

  /**
   * Return the contour levels.
   * @return An array containing the contour levels
   */
  public double[] getLevels() {
    return levels;
  }

  /**
   * If true the limits of the plot will be the grid limits.
   * If false the limits of the plot will be the contours.
   * @param b boolean
   */

  public void setLimitsToGrid(boolean b) {
    gridLimits = b;
  }

  /**
   *  Set the contour levels that are to have labels.
   * <pre>
   *    if 0 no labels are drawn
   *    if 1 every level gets a label
   *    If 2 every 2nd level gets a label
   *    etc.
   * </pre>
   */
  public void setLabelLevels(int i) {
    if (i <= 0)
      labelLevels = 0;
    else
      labelLevels = i;
  }

  /**
   * If true contour levels are calculated on a log scale.
   * @param b boolean
   */
  public void setLogLevels(boolean b) {
    logLevels = b;

    if (zmin <= 0.0 || zmax <= 0.0) logLevels = false;
  }

  /**
   * Set the number of contour levels.
   * @@param l Number of contour levels
   */
  public void setNLevels(int l) {
    if (l <= 0) return;

    levels = new double[l];

    calcLevels();

    detachCurves();
    curves = null;
  }

  /**
   * If true contour levels are calculated automatically.
   * @param b boolean
   */
  public void setAutoLevels(boolean b) {
    autoLevels = b;
  }

  /**
   * If true contour levels are not labeled.
   * @param b boolean
   */
  public void setDrawLabels(boolean b) {
    drawlabels = b;
  }

  /**
   * Set the label style, either TextLine.SCIENTIFIC or
   * TextLine.ALGEBRAIC.
   * @param s Style
   */
  public void setLabelStyle(int s) {
    labelStyle = s;
    calcLabels();
  }

  /**
   * Get the label style, either TextLine.SCIENTIFIC or
   * TextLine.ALGEBRAIC.
   * @return style
   */
  public int getLabelStyle() {
    return labelStyle;
  }

  /**
   * Set the label precision.
   * @param s Precision
   */
  public void setLabelPrecision(int p) {
    labelPrecision = p;
    calcLabels();
  }

  /**
   * Get the label precision.
   * @return precision
   */
  public int getLabelPrecision() {
    return labelPrecision;
  }

  /**
   * Set the label significant figures.
   * @param s number of significant figures
   */
  public void setLabelSignificance(int s) {
    labelSignificant = s;
    calcLabels();
  }

  /**
   * Get the number of significant figures for labels.
   * @return number of significant figures
   */
  public int getLabelSignificance() {
    return labelSignificant;
  }


  /**
   *   Add extra events to the G2Dint event handler.
   *
   *   If 'l' is pressed repaint without the labels
   *   If 'L' is pressed repaint with the labels.
   */
  public boolean keyDown(Event e, int key) {
    if (xaxis == null || yaxis == null) return false;

    if (super.keyDown(e, key)) return true;

    switch (key) {

      case 'l':
        drawlabels = false;
        repaint();
        return true;
      case 'L':
        drawlabels = true;
        repaint();
        return true;
    }

    return false;
  }

/*
********************
**
** Private Methods
**
*******************/

/*
**  calcLevels()
**              Calculate the contour levels
*/
  private void calcLevels() {
    int i;
    int l;

    if (!autoLevels) return;

    if (levels == null) levels = new double[NLEVELS];
    labels = new TextLine[levels.length];
    // Nice label steps not implemented yet
    //levelStep();


    if (logLevels) {
      double inc = Math.log(zmax - zmin) /
              (double) (levels.length + 1);
      try {
        for (i = 0; i < levels.length; i++)
          levels[i] = zmin +
                  Math.pow(Math.E, (double) (i + 1) * inc);
      } catch (Exception e) {
        Graph2D.out.println("Error calculateing Log levels!");
        Graph2D.out.println("... calculating linear levels instead");
        logLevels = false;
        calcLevels();
      }
    } else {
      double inc = (zmax - zmin) / (double) (levels.length + 1);
      for (i = 0; i < levels.length; i++) levels[i] = zmin + (double) (i + 1) * inc;
    }
  }
/*
**  calcLabels()
**              Calculate the labels
*/
  private void calcLabels() {
    int i;
    if (!autoLabels) return;

    if (levels == null || levels.length <= 0) return;

    labels = new TextLine[levels.length];


    for (i = 0; i < labels.length; i++) {
      labels[i] = new TextLine();
      labels[i].parseDouble(levels[i],
              labelSignificant, labelPrecision, labelStyle);
    }
  }

/*
**   zrange()
**           Calculate the range of the grid
*/

  private void zrange() {
    int i;

    zmin = grid[0];
    zmax = grid[1];
    for (i = 0; i < grid.length; i++) {

      zmin = Math.min(zmin, grid[i]);
      zmax = Math.max(zmax, grid[i]);

    }

    Graph2D.out.println("Data range: zmin=" + zmin + ", zmax=" + zmax);

    if (zmin == zmax) {
      Graph2D.out.println("Cannot produce contours of a constant surface!");
    }

    if (zmin <= 0 || zmax <= 0) logLevels = false;


  }
/*
**   paintFirst(Graphics g, Rectangle r)
**        before anything is painted calculate the contours.
*/

  public void paintFirst(Graphics g, Rectangle r) {

//         System.out.println("paintFirst called");



    if (curves == null && !noContours) {
      calculateCurves();
      calcLabels();
    }

    setContourColors();

    if (gridLimits && !userlimits) {
      if (xaxis != null) {
        if (xaxis.minimum > xmin) xaxis.minimum = xmin;
        if (xaxis.maximum < xmax) xaxis.maximum = xmax;
      }

      if (yaxis != null) {
        if (yaxis.minimum > ymin) yaxis.minimum = ymin;
        if (yaxis.maximum < ymax) yaxis.maximum = ymax;
      }
    } else if (dataset.isEmpty()) {
      if (xaxis != null) {
        xaxis.minimum = xmin;
        xaxis.maximum = xmax;
      }

      if (yaxis != null) {
        yaxis.minimum = ymin;
        yaxis.maximum = ymax;
      }
    }


//         System.out.println("paintFirst exit");
  }

  /**
   *  Set the colors for the contour lines
   */
  private void setContourColors() {
    int i;
    int j;
    Vector v;

    if (curves == null ||
            (contourColor == null && labelledColor == null))
      return;

    for (i = 0; i < curves.length; i++) {
      setContourColors(curves[i], null);
    }

    if (contourColor != null) {
      for (i = 0; i < curves.length; i++) {
        setContourColors(curves[i], contourColor);
      }
    }

    if (labelledColor != null) {
      for (i = 0; i < curves.length; i++) {
        if (i % labelLevels == 0) {
          setContourColors(curves[i], labelledColor);
        }
      }
    }


  }

  /**
   *  Set the colors for the contour lines
   */
  private void setContourColors(Vector v, Color c) {
    int i;
    DataSet d;

    if (v == null) return;

    for (i = 0; i < v.size(); i++) {
      d = (DataSet) (v.elementAt(i));
      if (d != null) d.linecolor = c;
    }

  }
/*
**    attachCurves()
**        Attach all the curves to the graph and to the axes
*/
  private void attachCurves() {
    int i;
    if (curves == null) return;

    for (i = 0; i < curves.length; i++) attachCurves(curves[i]);

  }
/*
**    attachCurves(Vector v)
**        Attach all the curves from a given level to the graph and to the axes
*/

  private void attachCurves(Vector v) {
    int j;
    if (v == null) return;
    for (j = 0; j < v.size(); j++) {
      attachDataSet((DataSet) (v.elementAt(j)));
      if (xaxis != null)
        xaxis.attachDataSet((DataSet) (v.elementAt(j)));
      if (yaxis != null)
        yaxis.attachDataSet((DataSet) (v.elementAt(j)));
    }
  }
/*
**    detachCurves()
**                 Detach All the curves from the graph and the axes.
*/
  private void detachCurves() {
    int i;
    if (curves == null) return;

    for (i = 0; i < curves.length; i++) detachCurves(curves[i]);
  }
/*
**    detachCurves()
**                 Detach all the curves from a given level from
**                 the graph and the axes.
*/
  private void detachCurves(Vector v) {
    int j;
    if (v == null) return;
    for (j = 0; j < v.size(); j++) {
      detachDataSet((DataSet) (v.elementAt(j)));
      if (xaxis != null)
        xaxis.detachDataSet((DataSet) (v.elementAt(j)));
      if (yaxis != null)
        yaxis.detachDataSet((DataSet) (v.elementAt(j)));
    }
  }

/*
**    paintLast(Graphics g, Rectangle rect)
**          Last thing to be done is to draw the contour labels if required.
*/
  public void paintLast(Graphics g, Rectangle rect) {
    int i, j;
    int points;
    int index;
    Vector v;
    DataSet ds;
    double point[] = new double[2];
    int x;
    int y;
    Color current = g.getColor();
    Rectangle r = new Rectangle();

    drawlabels = false;
    if (xaxis == null || yaxis == null || labels == null ||
            labelLevels == 0 || !drawlabels || curves == null) {
//                  System.out.println("paint last");
      super.paintLast(g, rect);
      return;
    }


    for (i = 0; i < levels.length; i++) {
      if (labels[i] != null && !labels[i].isNull() &&
              i % labelLevels == 0) {
        labels[i].setFont(labelfont);
        labels[i].setColor(labelcolor);
        v = curves[i];
        for (j = 0; j < v.size(); j++) {
          ds = (DataSet) (v.elementAt(j));
          points = ds.dataPoints();
          index = (int) (Math.random() * (double) MINCELLS);
          while (points > MINCELLS) {
            point = ds.getPoint(index);
            x = xaxis.getInteger(point[0]);
            y = yaxis.getInteger(point[1]);

            r.width = labels[i].getWidth(g);
            r.height = labels[i].getAscent(g);
            r.x = x - r.width / 2;
            r.y = y - r.height / 2;

            g.setColor(DataBackground);
            g.fillRect(r.x, r.y, r.width, r.height);

            g.setColor(current);

            labels[i].draw(g, r.x, r.y + r.height,
                    TextLine.LEFT);

            points -= MINCELLS;
            index += MINCELLS;
          }
        }
      }
    }

    super.paintLast(g, rect);


  }
/*
**   calculateCurves()
**        Calculate the contours and attach them to the graph and axes.
*/

  protected void calculateCurves() {
    int i;
    int j;
    double data[];
    double xscale = (xmax - xmin) / (double) (nx - 1);
    double yscale = (ymax - ymin) / (double) (ny - 1);

    IsoCurve isocurve;


    isocurve = new IsoCurveL(grid, nx, ny);
//          isocurve = new IsoCurve(grid,nx,ny);

    if (curves != null) {
      detachCurves();
      curves = null;
    }
    if (zmin == zmax) return;

    curves = new Vector[levels.length];


    for (i = 0; i < levels.length; i++) {
//              System.out.println("Calculating Contours: level="+levels[i]);
      isocurve.setValue(levels[i]);

      curves[i] = new Vector();

      while ((data = isocurve.getCurve()) != null) {
        for (j = 0; j < data.length;) {
          data[j] = xmin + data[j] * xscale;
          j++;
          data[j] = ymin + data[j] * yscale;
          j++;
        }

        try {
          curves[i].addElement(new ContourDataSet(data, data.length / 2));
//                  curves[i].addElement(new DataSet(data, data.length/2));
        } catch (Exception e) {
          Graph2D.out.println("Error loading contour into DataSet!");
          Graph2D.out.println("...Contour Level " + levels[i]);
        }
      }

      attachCurves(curves[i]);

      //repaint();
    }


  }


}
