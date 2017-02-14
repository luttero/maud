package it.unitn.ing.jgraph;

import it.unitn.ing.rista.util.Misc;

import java.awt.*;
import java.io.*;
import java.util.*;
import java.lang.*;


/*
**************************************************************************
**
**                      Class  graph.Marker
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
** class Marker extends Object
**        This class is designed to help install and manipulate
** markers
**
*************************************************************************/


/**
 * This class installs, manipulates and draws markers.
 * Markers are stroked using the line drawing method of the class Graph.
 * This means that any stroked figure can become a marker.
 *
 * @version $Revision: 1.2 $, $Date: 1970/01/04 19:14:42 $
 * @author  Leigh Brookshaw
 */

public class Markers extends Object {

/*
**********************
**
** Protected Variables
**
*********************/

  /**
   *    index of the last marker loaded
   */
  protected int last;
  /**
   *    maximum number of markers allowed
   */
  protected int max = 24;
  /**
   *    An array of vectors. Each element in the array contains the vertex
   *    vectors for a marker. Marker 1 is at element vert[0].
   */
  protected Vector<MarkerVertex> vert[];

/*
*******************
**
** Constructors
**
******************/
  /**
   *  The class contructor
   */
  public Markers() {
    last = 0;
    vert = new Vector[max];
  }

  /**
   * Instantiate the Marker class and load marker definitions from the parsed URL.
   * The format of the file is easily worked out from the
   * default marker file <a href="marker.txt">marker.txt</a>.
   *
   * @param file    The URL of the data file to read
   * @exception  IOException if there is an error with the IO stream.
   */
  public Markers(String file) throws IOException {

    this();

    LoadMarkers(file);
  }

  /**
   * Add the definition of a new marker. The arrays contain the vertex
   * points of the marker. The boolean array is used to define a relative draw
   * or move to the vertex.
   * The first vertex should always be a move (The boolean array is "true"
   * for a relative draw.
   *
   * @param m The index of the marker. The first marker has index 1.
   * @param n The number of vertices required to stroke the marker.
   * @param draw Boolean array containing relative move/draw instructions.
   *   "true" is a draw.
   * @param x Integer array containing the pixel x position of the vertices.
   *          All positions are relative to the center of the marker.
   * @param y Integer array containing the pixel y postions of the vertices.
   */
  public void AddMarker(int m, int n, boolean draw[], int x[], int y[]) {
    MarkerVertex v;
    int i;

    if (m < 1 || m > max) return;
    if (n <= 0) return;

    m--;
    last = m;
    vert[m] = new Vector<MarkerVertex>();

    for (i = 0; i < n; i++) {
      v = new MarkerVertex();

      v.draw = draw[i];
      v.x = x[i];
      v.y = y[i];

      vert[m].addElement(v);
    }

  }

  /**
   * Add the definition of a new marker. The new marker is appended onto
   * the marker list. The center of the marker is assumed to be at (0,0).
   * @param n number of move/draw commands
   * @param draw <i>true</i> if the point is to drawn to, <i>false</i> if
   * the point is to be moved to.
   * @param x X pixel to move/draw to.
   * @param x Y pixel to move/draw to.
   */
  public void AddMarker(int n, boolean draw[], int x[], int y[]) {
    AddMarker(last + 1, n, draw, x, y);
  }

  /**
   *  Delete the marker with the given index. the first marker has index 1.
   * @param n The index of the marker to delete. Markers start at index 1.
   */
  public void DeleteMarker(int n) {

    if (n < 1 || n > max) return;
    vert[n - 1] = null;

  }

  /**
   * Clear All markers.
   */
  public void ClearMarkers() {
    int i;

    if (last == 0) return;
    for (i = 0; i < max; i++) {
      vert[i] = null;
    }

    last = 0;

  }

  /**
   * This method reads the marker file and loads the marker definitions.
   * The format of the file is simple. The following are the keywords.
   * <dl>
   *   <dt><b>start</b>
   *   <dd> starts a new marker definition.
   *   <dt><b>end</b>
   *   <dd> ends a marker definition.
   *   <dt><b>m x y</b>
   *   <dd> move to position x,y
   *   <dt><b>l x y</b>
   *   <dd> line to position x,y
   * </dl>
   * All line drawing is relative to the previous position. The center
   * of the marker is assumed to be at (0,0).
   * As always blank lines are ignored and comments begin with a # character.
   *
   * @param file URL of file to load
   * @exception IOException If there is an IO error
   */
  public void LoadMarkers(String file) throws IOException {
	  StringTokenizer st;
    MarkerVertex v;
	  String line = null;

	  BufferedReader reader = Misc.getReader(file);
	  if (reader != null) {
		  line = reader.readLine();

		  while (line != null) {
			  if (!line.startsWith("#")) {
				  st = new StringTokenizer(line, "\r\n ,");
				  while (st.hasMoreTokens()) {
					  String result = st.nextToken();
					  if (result.equalsIgnoreCase("start")) {
						  vert[last] = new Vector();
					  } else if (result.equalsIgnoreCase("end")) {
						  last++;
					  } else if (result.equalsIgnoreCase("m")) {
						  v = new MarkerVertex();
						  v.draw = false;
						  if (st.hasMoreTokens()) {
						    v.x = Double.parseDouble(st.nextToken());
						    if (st.hasMoreTokens()) {
							    v.y = Double.parseDouble(st.nextToken());
//							    System.out.println(last);
								  vert[last].addElement(v);
							  }
						  }
					  } else if (result.equalsIgnoreCase("l")) {
						  v = new MarkerVertex();
						  v.draw = true;
						  if (st.hasMoreTokens()) {
							  v.x = Double.parseDouble(st.nextToken());
							  if (st.hasMoreTokens()) {
								  v.y = Double.parseDouble(st.nextToken());
								  vert[last].addElement(v);
							  }
						  }
					  }
				  }
			  }

			  line = reader.readLine();
		  }
	  }
  }

  /**
   * draw the marker
   * @param g Graphics context
   * @param m Index of the marker to draw
   * @param scale scale factor. All coordinates are multiplied by this factor.
   * @param x Coordinate where to draw the marker
   * @param y Coordinate where to draw the marker
   */
  public void draw(Graphics g, int m, double scale, double x, double y) {
    int i;
    MarkerVertex mv;
    int x0 = (int) x, y0 = (int) y, x1, y1;
    Vector v;


    if (m < 1 || m > max) return;
    if (scale <= 0) return;

    v = vert[m - 1];
    if (v == null) return;

    for (i = 0; i < v.size(); i++) {
      mv = (MarkerVertex) v.elementAt(i);

      if (mv.draw) {
        x1 = (int) x + (int) (mv.x * scale);
        y1 = (int) y + (int) (mv.y * scale);

        g.drawLine(x0, y0, x1, y1);

        x0 = x1;
        y0 = y1;
      } else {
        x0 = (int) x + (int) (mv.x * scale);
        y0 = (int) y + (int) (mv.y * scale);

      }

    }

  }
}


/**
 * This class is a structure class. It defines one vertex of the marker.
 */

class MarkerVertex extends Object {
  boolean draw;
  double x;
	double y;
}
