package it.unitn.ing.jgraph;

import java.util.*;
import java.lang.*;

/*
**************************************************************************
**
**    Class  IsoCurve
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
**    This class will calculate the curve of a given value passing
**    through a grid of values
**
*************************************************************************/


/**
 * This class will calculate the constant curve of a given value passing
 * through a grid of values.
 *
 * @version $Revision: 1.3 $, $Date: 2006/01/19 14:45:51 $.
 * @author Leigh Brookshaw
 */

public class IsoCurve extends Object {

/*
*****************
**
** Constants
**
****************/

  /**
   *  Flag a cell face as a terminal face ie the curve terminates here.
   */
  static final int TERMINAL = 0;

  /**
   *  Flag a cell as having the curve coming through its Left face
   */
  static final int LEFT = 1;

  /**
   *  Flag a cell as having the curve coming through its Right face
   */
  static final int RIGHT = 2;

  /**
   *  Flag a cell as having the curve coming through its Top face
   */
  static final int TOP = 3;

  /**
   *  Flag a cell as having the curve coming through its Bottom face
   */
  static final int BOTTOM = 4;
  /**
   *   Initial size of the array that will hold a contour.
   */
  static final int ARRAYSIZE = 100;
  /**
   *   Maximum size of the array that will hold a contour.
   */
  static final int MAXARRAYSIZE = 2000;

/*
***************
**
** Variables
**
**************/

  /**
   *   Vector of cells that the contour passes through.
   */
  protected Vector cells;
  /**
   *   Array holding the data grid.
   */
  protected double grid[];
  /**
   *   X Dimension of data grid
   */
  protected int nx;
  /**
   *   Y Dimension of data grid
   */
  protected int ny;
  /**
   *   Array that holds the points of the contour
   */
  protected double curve[];
  /**
   *   Number of points in the contour
   */
  protected int size;
  /**
   *   Contour value to be found
   */
  protected double value;

/*
*****************
**
** Constructors
**
****************/

  /**
   * Instantiate the class and initialize all the variables
   */
  public IsoCurve() {
    cells = null;
    grid = null;
    nx = 0;
    ny = 0;
    curve = null;
    size = 0;
    value = 0.0;
  }

  /**
   * Instantiate the class and initialize all the variables.
   * @param grid An nx by ny Array containing the grid to contour
   * @param nx   X dimension of the grid.
   * @param ny   Y dimension of the grid.
   */
  public IsoCurve(double grid[], int nx, int ny) {
    this();
    setGrid(grid, nx, ny);
  }

/*
******************
**
** Public Methods
**
*****************/
  /**
   * Set the grid to be contoured
   * @param nx   X dimension of the grid.
   * @param ny   Y dimension of the grid.
   */
  public void setGrid(double grid[], int nx, int ny) {
    this.grid = grid;
    this.nx = nx;
    this.ny = ny;
  }

  /**
   * Set the value to contour
   * @param value the contour level
   */
  public void setValue(double value) {
    this.value = value;

    if (grid == null) return;

    //System.out.println("setValue called:" + value);

    createCells();
  }

  /**
   * Return a contour curve. If null
   * is returned it means that all the contour curves have been found.
   * @return The array containing the (x,y) pairs of the contour curve.
   */
  public double[] getCurve() {
    size = 0;

    getcurve();

    if (size == 0 || curve == null) {
      //System.out.println("getCurve: Nothing found");
      return null;
    }

    double tmp[] = new double[size];

    System.arraycopy(curve, 0, tmp, 0, size);

    return tmp;
  }

/*
**********************
**
** Protected Methods
**
*********************/
  /**
   * Create the vector of all cells that contain the contour.
   */
  protected void createCells() {
    double bl, br, tl, tr;
    boolean bottom, top, right, left;
    Cell cell;
    int jcell, icell;
    int i, j, count;

    if (cells == null)
      cells = new Vector();
    else
      cells.removeAllElements();


    for (j = 0; j < ny - 1; j++) {

      jcell = j * nx;

      for (i = 0; i < nx - 1; i++) {

        icell = i + jcell;

        bl = grid[icell];
        br = grid[icell + 1];
        tl = grid[icell + nx];
        tr = grid[icell + nx + 1];

        bottom = false;
        top = false;
        left = false;
        right = false;

        if ((bl - value) * (br - value) <= 0.0) bottom = true;
        if ((br - value) * (tr - value) <= 0.0) right = true;
        if ((tr - value) * (tl - value) <= 0.0) top = true;
        if ((tl - value) * (bl - value) <= 0.0) left = true;

        if (top && bottom && left && right) {

          if (tr >= tl && bl >= br) {

            cell = new Cell();
            cell.i = i;
            cell.j = j;
            cell.face[0] = TOP;
            cell.face[1] = LEFT;
            cells.addElement(cell);

            cell = new Cell();
            cell.i = i;
            cell.j = j;
            cell.face[0] = BOTTOM;
            cell.face[1] = RIGHT;
            cells.addElement(cell);

          } else if (tl >= tr && br >= bl) {

            cell = new Cell();
            cell.i = i;
            cell.j = j;
            cell.face[0] = TOP;
            cell.face[1] = LEFT;
            cells.addElement(cell);

            cell = new Cell();
            cell.i = i;
            cell.j = j;
            cell.face[0] = BOTTOM;
            cell.face[1] = RIGHT;


          }
        } else if (top || bottom || left || right) {
          cell = new Cell();
          cells.addElement(cell);
          cell.i = i;
          cell.j = j;

          if (bl == value && br == value) bottom = false;
          if (bl == value && tl == value) left = false;
          if (tr == value && tl == value) top = false;
          if (tr == value && br == value) right = false;
/*
**                          Check for pathological cell and to avoid out of bounds
**                          errors!
*/

          count = 0;
          if (bottom) count++;
          if (top) count++;
          if (left) count++;
          if (right) count++;

          if (count > 2) {
            Graph2D.out.println("IsoCurve: Pathological Cell");
            Graph2D.out.println("          value=" + value);
            Graph2D.out.println("          i=" + i + ", j=" + j);
            Graph2D.out.println("          tl=" + tl + ", tr=" + tr);
            Graph2D.out.println("          bl=" + bl + ", br=" + br);
          } else {
            count = 0;
            if (bottom) {
              cell.face[count] = BOTTOM;
              count++;
            }
            if (top) {
              cell.face[count] = TOP;
              count++;
            }
            if (left) {
              cell.face[count] = LEFT;
              count++;
            }
            if (right) {
              cell.face[count] = RIGHT;
              count++;
            }
          }
        }


      }
    }
  }

  /**
   * Traverse the cells and find One connect countour.
   */
  protected void getcurve() {
    Cell current;
    int face = TERMINAL;
    int ifcell = -1;
    int jfcell = -1;
    int icell,jcell;
    double bl, br, tl, tr;
    int index;
    int i;
    double d[] = new double[2];


    size = 0;

    //System.out.println("getcurve: Number of cells "+cells.size());

    if (cells == null || cells.isEmpty()) return;

    current = (Cell) (cells.firstElement());

    if (current.face[0] == TERMINAL && current.face[1] == TERMINAL) {
      cells.removeElement(current);
      Graph2D.out.println("getcurve: Terminal cell removed!");
      return;
    }
/*
**          Calculate the first point in the new curve
*/

    icell = current.i;
    jcell = current.j;

    ifcell = -1;
    jfcell = -1;
/*
**          Check to see if we can loop back on ourselves. ie we have partially
**          resolved this curve or we are against a wall.
**          Use the terminating wall to start the curve.
*/
    if (search(icell - 1, jcell) == null &&
            (current.face[0] == LEFT || current.face[1] == LEFT)) {

      addDataPoint(LEFT, icell, jcell);
      face = LEFT;

    } else if (search(icell + 1, jcell) == null &&
            (current.face[0] == RIGHT || current.face[1] == RIGHT)) {

      addDataPoint(RIGHT, icell, jcell);
      face = RIGHT;

    } else if (search(icell, jcell - nx) == null &&
            (current.face[0] == BOTTOM || current.face[1] == BOTTOM)) {

      addDataPoint(BOTTOM, icell, jcell);
      face = BOTTOM;

    } else if (search(icell, jcell + nx) == null &&
            (current.face[0] == TOP || current.face[1] == TOP)) {

      addDataPoint(TOP, icell, jcell);
      face = TOP;

    } else {

/*
**          No terminating wall so it does not matter where we start as
**          long as it is not a terminal face. Remember the position
**          of the cell behind us just in case we loop back as the curve
**          must be closed.
*/
      index = 0;
      if (current.face[0] == TERMINAL) index = 1;

      addDataPoint(current.face[index], icell, jcell);
      face = current.face[index];


      if (face == TOP) {
        ifcell = current.i;
        jfcell = current.j + 1;
      } else if (face == BOTTOM) {
        ifcell = current.i;
        jfcell = current.j - 1;
      } else if (face == LEFT) {
        ifcell = current.i - 1;
        jfcell = current.j;
      } else if (face == RIGHT) {
        ifcell = current.i + 1;
        jfcell = current.j;
      }
    }


    while (current != null) {

      icell = current.i;
      jcell = current.j;

      if (current.face[0] == face)
        face = current.face[1];
      else
        face = current.face[0];

      if (face != TERMINAL) addDataPoint(face, icell, jcell);

      if (face == TOP) {
        jcell++;
        face = BOTTOM;
      } else if (face == BOTTOM) {
        jcell--;
        face = TOP;
      } else if (face == LEFT) {
        icell--;
        face = RIGHT;
      } else if (face == RIGHT) {
        icell++;
        face = LEFT;
      }

      cells.removeElement(current);

      if (icell == ifcell && jcell == jfcell) {
        addDataPoint(curve[0], curve[1]);
        current = null;
      } else {
        current = search(icell, jcell);
      }
    }

  }

  /**
   * Return the (x,y) position where the contour cross the cell wall.
   * @param wall The cell wall the contour crosses.
   * @param icell The x index of the cell position.
   * @param jcell The y index of the cell position.
   */

  protected double[] getPoint(int wall, int icell, int jcell) {
    double d[] = new double[2];
    double bl, br, tl, tr;
    int index = icell + jcell * nx;

    if (wall == TOP) {
      tl = grid[index + nx];
      tr = grid[index + nx + 1];
      d[1] = (double) (jcell + 1);
      d[0] = (double) icell + (value - tl) / (tr - tl);
    } else if (wall == BOTTOM) {
      bl = grid[index];
      br = grid[index + 1];
      d[1] = (double) jcell;
      d[0] = (double) icell + (value - bl) / (br - bl);
    } else if (wall == LEFT) {
      bl = grid[index];
      tl = grid[index + nx];
      d[1] = (double) jcell + (value - bl) / (tl - bl);
      d[0] = (double) icell;
    } else if (wall == RIGHT) {
      br = grid[index + 1];
      tr = grid[index + 1 + nx];
      d[1] = (double) jcell + (value - br) / (tr - br);
      d[0] = (double) (icell + 1);
    }

    return d;
  }

  /**
   * Add a data point to the contour curve.
   * @param wall The cell wall the contour crosses.
   * @param icell The x index of the cell position.
   * @param jcell The y index of the cell position.
   */
  protected void addDataPoint(int wall, int icell, int jcell) {
    double d[];

    d = getPoint(wall, icell, jcell);

    addDataPoint(d[0], d[1]);
  }


  /**
   * Add a data point to the contour curve.
   * @param x The x position of the point.
   * @param y The y position of the point.
   */
  protected void addDataPoint(double x, double y) {

    if (size >= MAXARRAYSIZE - 2) return;

    if (curve == null) {
      curve = new double[ARRAYSIZE];
    } else if (size == curve.length - 2) {
      double tmp[] = new double[2 * size];
      System.arraycopy(curve, 0, tmp, 0, size);
      curve = tmp;
    }

    curve[size] = x;
    size++;
    curve[size] = y;
    size++;
  }

  /**
   * Search for a cell in the contour cell list
   * @param icell the x index of the cell
   * @param jcell the y index of the cell
   */
  protected Cell search(int icell, int jcell) {
    int i;
    Cell current = null;

    if (cells.isEmpty()) return null;

    for (i = 0; i < cells.size(); i++) {
      current = (Cell) (cells.elementAt(i));

      if (current.i == icell && current.j == jcell) return current;
    }

    return null;

  }


}

/**
 * This class is a structure class holding information about the
 * cells that the contour line passes through. Used only with IsoCurve class
 */
class Cell extends Object {
  int i;
  int j;
  int face[] = new int[2];
  boolean visited;

  public Cell() {
    i = -1;
    j = -1;
    face[0] = IsoCurve.TERMINAL;
    face[1] = IsoCurve.TERMINAL;
  }

}
