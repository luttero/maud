package it.unitn.ing.jgraph;

import java.applet.*;
import java.lang.*;
import java.io.InputStream;
import java.net.URL;

/*
**************************************************************************
**
**    Class  LoadData
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
**    This class will load data from a given URL
**
*************************************************************************/

/**
 * This class will load data (as a seperate thread) into a DataSet
 * given a URL.
 *
 * @version  $Revision: 1.3 $, $Date: 2006/01/19 14:45:51 $
 * @author   Leigh Brookshaw
 */


public class LoadData extends Thread {

/*
********************
** Public Variables
*******************/

  /**
   * Flag used to specify the type of data to be loaded. That is purely
   * numerical.
   */

  public final int NUMERIC = 1;

  /**
   * Flag used to specify the type of data to be loaded. That is
   * can contain non-numerical data. Not Implemented
   */
  public final int OBJECT = 2;


/*
*********************
** Private Variables
********************/

/*
**  Places were we can send error/informational messages
*/
  private Graph2D graph = null;
  private Applet applet = null;
/*
** The URL where we are to get the data
*/
  private URL file;
/*
**  Number of data points in temporary storage.
*/
  private int Max = 250;

/*
** The DataSet we will load the data into
*/
  private DataSet ds = null;
/*
** Optional array to load data into
*/
  private double array[] = null;

/*
** Boolean - has the data load finished
*/
  private boolean finished = false;
/*
** Boolean - has the data load started
*/
  private boolean started = false;
/*
** number of data points.
*/
  private int count = 0;
/*
** Data type
*/
//  private int dataType = NUMERIC;

/*
****************
** Constructors
****************/

  /**
   * Instantiate the class
   */

  public LoadData() {
    finished = false;
    started = false;
  }

  /**
   * Instantiate the class
   *
   * @param d DataSet to load the data into.
   */

  public LoadData(DataSet d) {
    ds = d;
    finished = false;
    started = false;
  }

/*
*****************
** Public Methods
*****************/


  /**
   * Start loading the data into a/the DataSet.
   *
   * @param file URL of data file
   * @return The DataSet that the data will be loaded into
   */

  public DataSet loadDataSet(URL file) {
    if (file == null) return null;
    if (ds == null) ds = new DataSet();

    this.file = file;
    this.start();
    return ds;
  }

  /**
   * Start loading the data into an Array.
   *
   * @param file URL of data file
   */
  public void loadArray(URL file) {
    if (file == null) return;
    this.file = file;
    this.start();
  }

  /**
   * Start loading the data into a/the DataSet.
   *
   * @param file URL of data file
   * @param drawable An object that can be drawn to that will indicate
   * that data is loading. eg. An applet or the Graph2D canvas.
   */
  public DataSet loadDataSet(URL file, Object drawable) {
    if (file == null) return null;
    if (ds == null) ds = new DataSet();
    this.file = file;
    if (drawable != null) {
      if (drawable instanceof Applet) {
        applet = (Applet) drawable;
      } else if (drawable instanceof Graph2D) {
        graph = (Graph2D) drawable;
        graph.attachDataSet(ds);
      }
    }
    this.start();
    return ds;
  }

  /**
   * Start loading the data into an array.
   *
   * @param file URL of data file
   * @param drawable An object that can be drawn to that will indicate
   * that data is loading. eg. An applet or the Graph2D canvas.
   */

  public void loadArray(URL file, Object drawable) {
    if (file == null) return;
    this.file = file;
    if (drawable != null) {
      if (drawable instanceof Applet) {
        applet = (Applet) drawable;
      } else if (drawable instanceof Graph2D) {
        graph = (Graph2D) drawable;
        graph.attachDataSet(ds);
      }
    }
    this.start();
    return;
  }

  /**
   * The method to be run as a seperate thread. It does all the work
   */
  public void run() {
    int datamax = 2 * Max;
    byte b[] = new byte[50];
    int nbytes = 0;
    double data[] = new double[datamax];
    int size = 0;

    InputStream is = null;
    boolean comment = false;
    int c;

    setPriority(Thread.MIN_PRIORITY);

    try {
      is = file.openStream();

      started = true;
      count = 0;

      if (graph != null) graph.startedloading();

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
              data[size] = Double.valueOf(s).doubleValue();
              size++;
              if (size >= datamax) {
                append(data, size);
                size = 0;
                Graph2D.out.println(
                        "Loading Data! Points Loaded " +
                        count);
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

      if (size > 1) append(data, size);

      if (is != null) is.close();
    } catch (Exception e) {
      printmessage("Error loading data!");
      return;
    }


    started = false;
    finished = true;

    if (graph != null) graph.finishedloading();

    if (count == 0) {
      printmessage("Zero data loaded!");
      return;
    }


  }

  /**
   * @return The DataSet that is being filled.
   */
  public DataSet getDataSet() {
    return ds;
  }

  /**
   * @return The loaded data array.
   */
  public synchronized double[] getArray() {
    if (array == null || array.length == 0) return null;
    if (finished) return array;
    if (started) {
      double tmp[] = new double[array.length];
      System.arraycopy(array, 0, tmp, 0, array.length);
      return tmp;
    }

    return null;
  }

  /**
   * @return true if the data has started loading.
   */
  public boolean started() {
    return started;
  }

  /**
   * @return true if the data has finished loading.
   */
  public boolean finished() {
    return finished;
  }

  /**
   * @return The current size of the data array.
   */
  public int length() {
    return count;
  }

  /*
  ** Append the loaded data to either the DataSet or the Array.
  */

  private synchronized void append(double data[], int size) {

    if (size == 0) return;
    count += size;

    if (ds != null) {
      try {
        ds.append(data, size / 2);
      } catch (Exception e) {
        printmessage("Failed to append data to DataSet!");
      }
    } else if (array != null) {
      double tmp[] = new double[array.length + size];
      System.arraycopy(array, 0, tmp, 0, size);
      System.arraycopy(data, 0, tmp, array.length, size);
      array = tmp;
    } else {
      array = new double[size];
      System.arraycopy(data, 0, array, 0, size);
    }

    return;
  }

  /*
  ** Print out a message - if possible
  */

  private void printmessage(String s) {

    if (s == null) return;

    if (applet != null) applet.showStatus(s);

    Graph2D.out.println(s);


  }


}
