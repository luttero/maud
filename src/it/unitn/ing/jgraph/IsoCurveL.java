package it.unitn.ing.jgraph;

import java.lang.*;

/*
**************************************************************************
**
**    Class  IsoCurveL
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
**    This class will calculate the curve of a given value passing
**    through a grid of values
**
*************************************************************************/


/**
 * This class will calculate the constant curve of a given value passing
 * through a grid of values.
 *
 * @version $Revision: 1.3 $, $Date: 2006/01/19 14:45:51 $.
 * @author Luca Lutterotti
 */

public class IsoCurveL extends IsoCurve {

/*
*****************
**
** Constructors
**
****************/

  /**
   * Instantiate the class and initialize all the variables
   */
  public IsoCurveL() {
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
  public IsoCurveL(double grid[], int nx, int ny) {
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
   * Set the value to contour
   * @param value the contour level
   */
  public void setValue(double value) {
    this.value = value;

    if (grid == null) return;

    //System.out.println("setValue called:" + value);

    size = 0;
    createCells();

  }

  /**
   * Return a contour curve. If null
   * is returned it means that all the contour curves have been found.
   * @return The array containing the (x,y) pairs of the contour curve.
   */
  public double[] getCurve() {

    if (size == 0 || curve == null) {
      //System.out.println("getCurve: Nothing found");
      return null;
    }

    double tmp[] = new double[size];

    System.arraycopy(curve, 0, tmp, 0, size);

    size = 0;
    curve = null;

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
    int jcell, icell;
    int i, j, count;

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

        double blv = bl - value;
        double brv = br - value;
        double tlv = tl - value;
        double trv = tr - value;

        if (blv * brv <= 0.0) bottom = true;
        if (brv * trv <= 0.0) right = true;
        if (trv * tlv <= 0.0) top = true;
        if (tlv * blv <= 0.0) left = true;

        if (top && bottom && left && right) {

          boolean TLshorter = (Math.abs(tlv) * Math.abs(trv) <= Math.abs(trv) * Math.abs(blv));

          if (TLshorter) {
            addDataPoint(TOP, i, j);
            addDataPoint(LEFT, i, j);
          } else {
            addDataPoint(TOP, i, j);
            addDataPoint(RIGHT, i, j);
          }
          if (TLshorter) {
            addDataPoint(BOTTOM, i, j);
            addDataPoint(RIGHT, i, j);
          } else {
            addDataPoint(BOTTOM, i, j);
            addDataPoint(LEFT, i, j);
          }

        } else if (top || bottom || left || right) {

          if (bl == value && br == value) bottom = false;
          if (bl == value && tl == value) left = false;
          if (tr == value && tl == value) top = false;
          if (tr == value && br == value) right = false;

//                          Check for pathological cell and to avoid out of bounds errors!

          count = 0;
          if (bottom) count++;
          if (top) count++;
          if (left) count++;
          if (right) count++;

          if (count == 2) {

            int tcount = 0;
            int first = TERMINAL, last = TERMINAL;
            if (bottom) {
              first = BOTTOM;
              tcount++;
            }
            if (top && tcount == 0) {
              first = TOP;
              tcount++;
            } else if (top)
              last = TOP;
            if (left && tcount == 0) {
              first = LEFT;
              tcount++;
            } else if (left)
              last = LEFT;
            if (right)
              last = RIGHT;

            if (first != TERMINAL && last != TERMINAL) {

              addDataPoint(first, i, j);
              addDataPoint(last, i, j);
            }
          }
        }


      }
    }
  }


}

