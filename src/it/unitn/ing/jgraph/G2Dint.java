package it.unitn.ing.jgraph;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.Vector;

/*
**************************************************************************
**
**    Class  G2Dint
**
**************************************************************************
**    Copyright (C) 1995, 1996 Leigh Brookshaw
**    modify 1998 by Luca Lutterotti for swing support and event model
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
**    This class is an extension of Graph2D class.
**    It adds interactive selection of the plotting range
**    and can display the mouse position in user coordinates.
**
*************************************************************************/

/**
 *    This class is an extension of Graph2D class.
 *    It adds interactive selection of the plotting range
 *    and can display the mouse position in user coordinates.
 *
 *    <h4>Mouse Events</h4>
 *    <dl>
 *     <dt>MouseDown
 *     <dd>Starts the range selection
 *     <dt>MouseDrag
 *     <dd>Drag out a rectangular range selection
 *     <dt>MouseUp
 *     <dd>Replot with modified plotting range.
 *     <dt>
 *    </dl>
 *    <h4>Popup window (right mouse button or mouse click and apple key on Mac</h4>
 *    <dl>
 *     <dt>Reset
 *     <dd>Redraw plot with default limits
 *     <dt>Redraw
 *     <dd>Redraw plot using current limits
 *     <dt>Scale
 *     <dd>Pop window to enter manually plot range
 *     <dt>Show/Hide coordinates
 *     <dd>Toggle pop-up window that displays the mouse position
 *         in user coordinates
 *     <dt>Show closest data point
 *     <dd>Show coordinates of the closest data point to the cursor
 *    </dl>
 *    <P>
 *    <B>Note:</B> To hide Any pop-window use the close box in the
 *    window. This will hide the window at any time. Depending on your
 *    windowing system the mouse button might have to be pressed in the
 *    graphic window to ensure it has the keyboard focus.
 *
 * @version $Revision: 1.7 $, $Date: 2005/09/16 15:47:23 $.
 * @author Leigh Brookshaw
 * @author Luca Lutterotti
 */

public class G2Dint extends Graph2D {

  public boolean isLikeAltDown = false;

  /**
   *    Set to true when a rectangle is being dragged out by the mouse
   */
  protected boolean drag = false;
  /**
   *    User limits. The user has set the limits using the mouse drag option
   */
  protected boolean userlimits = false;

  /**
   *    The popup window for the cursor position command
   */
  static Gin cpgin = null;
  /**
   *    The popup window for the data point command
   */
  static Gin dpgin = null;
  /**
   *    The popup window to manually set the range
   */
  static PeakInfoFrame ppgin = null;
	/**
	 *    The popup window to manually set the range
	 */
  Range range = null;
  /**
   *    Button Down position
   */
  int x0,y0;
  /**
   *    Button Drag position
   */
  int x1,y1;
/*
**    Previous Button Drag position
*/
  int x1old, y1old;

  /**
   *    Attached X Axis which must be registered with this class.
   *    This is one of the axes used to find the drag range.
   *    If no X axis is registered no mouse drag.
   */
  protected Axis xaxis;
  /**
   *    Attached Y Axis which must be registered with this class.
   *    This is one of the axes used to find the drag range.
   *    If no Y axis is registered no mouse drag.
   */
  protected Axis yaxis;

  /**
   *    The popup window for the drop down menu
   */
  JPopupMenu popup = null;

	Vector<PeakInfo> peakInfosVector = null;

	public static int DIFFRACTION_PLOT = 0, REFLECTIVITY_PLOT = 1, FLUORESCENCE_PLOT = 2;

	public int plot_type = DIFFRACTION_PLOT;

  /**
   *    Create the graph canvas and add the mouse listeners
   */
  public G2Dint() {

    super();

    addMouseListener(new g2Dmouse());
    addMouseMotionListener(new g2Dmousemotion());
  }

  /**
   *    Create the graph canvas and add the mouse listeners
   */
  public G2Dint(MouseAdapter aMouseListener, MouseMotionAdapter aMouseMotionListener) {

    super();

    if (aMouseListener != null)
      addMouseListener(aMouseListener);
    if (aMouseMotionListener != null)
      addMouseMotionListener(aMouseMotionListener);
  }

  /**
   *    Create Xaxis to be used for the drag scaling
   */
  public Axis createAxis(int value) {
    if (value == Axis.BOTTOM) {
      xaxis = super.createAxis(Axis.BOTTOM);
      return xaxis;
    } else if (value == Axis.LEFT) {
      yaxis = super.createAxis(Axis.LEFT);
      return yaxis;
    } else
      return super.createAxis(value);
  }

  /**
   *    Create Xaxis to be used for the drag scaling
   */
  public Axis createXAxis() {
    xaxis = super.createAxis(Axis.BOTTOM);
    return xaxis;
  }

  /**
   *    Create Yaxis to be used for the drag scaling
   */
  public Axis createYAxis() {
    yaxis = super.createAxis(Axis.LEFT);
    return yaxis;
  }

  /**
   *    Attach axis to be used for the drag scaling. X axes are assumed to
   *    have Axis position Axis.BOTTOM or Axis.TOP. Y axes are assumed
   *    to have position Axis.LEFT or Axis.RIGHT.
   * @param a Axis to attach
   * @see Axis
   */
  public void attachAxis(Axis a) {
    if (a == null) return;

    super.attachAxis(a);

    if (a.getAxisPos() == Axis.BOTTOM || a.getAxisPos() == Axis.TOP) {
      xaxis = a;
    } else {
      yaxis = a;
    }
  }


	public void attachPeaksInfos(Vector<PeakInfo> peaksInfos) {
		peakInfosVector = peaksInfos;
	}

	public Vector<PeakInfo> getPeakInfosVector() {
		return peakInfosVector;
	}

	int infoDefaultLength = 10;
	PeakInfo[] infos = null;
	double lastCoordX = -9999999.999999;

	public void setInfoDefaultLength(int value) {
		infoDefaultLength = value;
		infos = null;
	}

	public PeakInfo[] getPeakInfoForCoordinate(double coordX) {
		if (peakInfosVector == null || peakInfosVector.size() < 1)
			return null;
		if (infos == null) {
			infos = new PeakInfo[infoDefaultLength];
			lastCoordX = coordX + 1.0;
		}
		if (infos.length > peakInfosVector.size()) {
			infos = new PeakInfo[peakInfosVector.size()];
			lastCoordX = coordX + 1.0;
		}
		if (coordX != lastCoordX) {
			// rebuild list
			int size = infos.length;
			for (int i = 0; i < size; i++) {
				peakInfosVector.get(i).calculateDiff(coordX);
				infos[i] = peakInfosVector.get(i);
			}
			orderInfos();
			double lastDiff = infos[size - 1].diff;
			for (int i = infos.length; i < peakInfosVector.size(); i++) {
				if (peakInfosVector.get(i).calculateDiff(coordX) < lastDiff) {
					lastDiff = insertInInfo(i);
				}
			}
		}
		return infos;
	}

	private void orderInfos() {
		// selection sorting
		for (int i = 0; i < infos.length - 1; i++) {
			int min = i;
			double minDiff = infos[min].diff;
			for (int j = i + 1; j < infos.length; j++)
				if (minDiff > infos[j].diff) {
					min = j;
					minDiff = infos[min].diff;
				}
			if (min != i) {
				PeakInfo peakInfo = infos[i];
				infos[i] = infos[min];
				infos[min] = peakInfo;
			}
		}
	}

	private double insertInInfo(int index) {
		int size = infos.length - 1;
		int insertPosition = size;
		double insertDiff = peakInfosVector.get(index).diff;
		for (int i = size - 1; i > -1; i--) {
			if (infos[i].diff < insertDiff)
				break;
			insertPosition--;
		}
		for (int i = size; i > insertPosition; i--)
			infos[i] = infos[i - 1];
		infos[insertPosition] = peakInfosVector.get(index);
		return infos[size].diff;
	}

  /**
   *  New update method incorporating mouse dragging.
   */
  public void update(Graphics g) {

    Rectangle r = getBounds();
    Color c = g.getColor();

    /* The r.x and r.y returned from bounds is relative to the
    ** parents space so set them equal to zero
          */
    r.x = 0;
    r.y = 0;

    if (drag) {
      /**
       * Set the dragColor. Do it everytime just incase someone
       * is playing silly buggers with the background color.
       */
      if (DataBackground != null) {
        g.setColor(DataBackground);

        float hsb[] = Color.RGBtoHSB(
              DataBackground.getRed(),
              DataBackground.getGreen(),
              DataBackground.getBlue(),
              null);

        if (hsb[2] < 0.5)
          g.setXORMode(Color.white);
        else
          g.setXORMode(Color.black);
      } else
        g.setXORMode(Color.black);

/*
**         Drag out the new box.
**         Use drawLine instead of drawRect to avoid problems
**         when width and heights become negative. Seems drawRect
**         can't handle it!
*/

/*
       ** Draw over old box to erase it. This works because XORMode
       ** has been set. If from one call to the next the background
             ** color changes going to get some odd results.
       */
      g.drawLine(x0, y0, x1old, y0);
      g.drawLine(x1old, y0, x1old, y1old);
      g.drawLine(x0, y1old, x1old, y1old);
      g.drawLine(x0, y0, x0, y1old);
/*
       ** draw out new box
       */
      g.drawLine(x0, y0, x1, y0);
      g.drawLine(x1, y0, x1, y1);
      g.drawLine(x0, y1, x1, y1);
      g.drawLine(x0, y0, x0, y1);
/*
       ** Set color back to default color
       */
      g.setColor(c);

      x1old = x1;
      y1old = y1;

      return;
    }

    if (clearAll) {
//      g.setColor(getBackground());
//      g.fillRect(r.x, r.y, r.width, r.height);
//      g.setColor(c);
    }
    if (paintAll)
      paint(g);


  }


  /**
   *   Find the closest data point to the cursor
   */
  public double[] getClosestPoint(int ix, int iy) {
    DataSet ds;
    int i;
    double a[] = new double[3];
    double distsq = -1.0;
    double data[] = {0.0, 0.0};
    double x = xaxis.getDouble(ix);
    double y = yaxis.getDouble(iy);

    for (i = 0; i < dataset.size(); i++) {
      ds = (DataSet) (dataset.elementAt(i));

      a = ds.getClosestPoint(x, y);

      if (distsq < 0.0 || distsq > a[2]) {
        data[0] = a[0];
        data[1] = a[1];
        distsq = a[2];
      }
    }
    return data;

  }

  public double[] getClosestPoint(double x) {
    DataSet ds;
    int i;
    double a[];
    double distsq = -1.0;
    double data[] = {0.0, 0.0};

    for (i = 0; i < dataset.size(); i++) {
      ds = (DataSet) (dataset.elementAt(i));

      a = ds.getClosestPoint(x);

      if (distsq < 0.0 || distsq > a[2]) {
        data[0] = a[0];
        data[1] = a[1];
        distsq = a[2];
      }
    }
    return data;

  }

  /**
   *   Find the coordinates of the cursor
   */
  public double[] getPoint(int ix, int iy) {
    double data[] = new double[2];
    data[0] = xaxis.getDouble(ix);
    data[1] = yaxis.getDouble(iy);

    return data;
  }

  public Frame getFrameParent() {
    Container aparent = getParent();
    while (!(aparent instanceof Frame) && aparent != null)
      aparent = aparent.getParent();
    return (Frame) aparent;
  }

  public double[] getRanges() {

    double[] trange = new double[4];

    trange[0] = xaxis.minimum;
    trange[1] = xaxis.maximum;
    trange[2] = yaxis.minimum;
    trange[3] = yaxis.maximum;

    return trange;
  }

  public void updateDataAndPaint(double[] trange) {

    xaxis.minimum = trange[0];
    xaxis.maximum = trange[1];
    yaxis.minimum = trange[2];
    yaxis.maximum = trange[3];

    userlimits = true;

    repaint();
  }

	public void repaintKeepZoom() {
		updateDataAndPaint(getRanges());
	}

  public void dispose() {
    if (cpgin != null && cpgin.isVisible()) {
      cpgin.setVisible(false);
      cpgin.dispose();
    }
    cpgin = null;
    if (dpgin != null && dpgin.isVisible()) {
      dpgin.setVisible(false);
      dpgin.dispose();
    }
    dpgin = null;
    if (range != null && range.isVisible()) {
      range.setVisible(false);
      range.dispose();
    }
    range = null;
  }

  public void resetRangeFull() {
    resetRange();
    userlimits = false;
    repaint();
  }

  public boolean userLimits() {
    return userlimits;
  }

  public void setUserLimits(double xmin, double xmax, double ymin, double ymax) {
    if (Math.abs(xmax - xmin) > 0 && Math.abs(ymax - ymin) > 0) {
      userlimits = true;
      if (xmin < xmax) {
        xaxis.minimum = xmin;
        xaxis.maximum = xmax;
      } else {
        xaxis.minimum = xmax;
        xaxis.maximum = xmin;
      }

      if (ymax > ymin) {
        yaxis.minimum = ymin;
        yaxis.maximum = ymax;
      } else {
        yaxis.minimum = ymax;
        yaxis.maximum = ymin;
      }

//      update(getGraphics());
      repaint();
    } else if(xmin == xmax && ymin == ymax) {
      resetRangeFull();
    }
  }

  public double getXaxisMinimum() {
    return xaxis.minimum;
  }

  public double getXaxisMaximum() {
    return xaxis.maximum;
  }

  public double getYaxisMinimum() {
    return yaxis.minimum;
  }

  public double getYaxisMaximum() {
    return yaxis.maximum;
  }

  public void altDownMousePressed(MouseEvent e) {

  }

  public void altDownMouseReleased(MouseEvent e) {

  }

  public void altDownMouseClicked(MouseEvent e) {

  }

  public void altDownMouseDragged(MouseEvent e) {

  }

  public void altDownMouseMoved(MouseEvent e) {

  }

  class g2Dmouse extends MouseAdapter {

    /**
     * Handle the Mouse Down events
     */

    public void mousePressed(MouseEvent e) {
      if (e.isAltDown() || (isLikeAltDown && e.isPopupTrigger())) {
        altDownMousePressed(e);
      } else if (e.isPopupTrigger())
        showPopup(e);
      else if (xaxis != null && yaxis != null) {

        /*
        ** Soon as the mouse button is pressed request the Focus
        ** otherwise we will miss key events
        */

        requestFocus();

        x0 = e.getX();
        y0 = e.getY();
        drag = true;
        x1old = x0;
        y1old = y0;

        if (x0 < datarect.x)
          x0 = datarect.x;
        else if (x0 > datarect.x + datarect.width)
          x0 = datarect.x + datarect.width;

        if (y0 < datarect.y)
          y0 = datarect.y;
        else if (y0 > datarect.y + datarect.height)
          y0 = datarect.y + datarect.height;
      }
    }

    /**
     * Handle the Mouse Up events
     */

    public void mouseReleased(MouseEvent e) {

      if (e.isAltDown() || (isLikeAltDown && e.isPopupTrigger())) {
        altDownMouseReleased(e);
      } else if (e.isPopupTrigger())
        showPopup(e);
      else if ((xaxis != null && yaxis != null) && drag) {

        x1 = e.getX();
        y1 = e.getY();

//        if (drag)
          userlimits = true;

        drag = false;

        if (x1 < datarect.x)
          x1 = datarect.x;
        else if (x1 > datarect.x + datarect.width)
          x1 = datarect.x + datarect.width;

        if (y1 < datarect.y)
          y1 = datarect.y;
        else if (y1 > datarect.y + datarect.height)
          y1 = datarect.y + datarect.height;

        if (Math.abs(x0 - x1) > 2 && Math.abs(y0 - y1) > 2) {
          if (x0 < x1) {
            double min = xaxis.getDouble(x0);
            double max = xaxis.getDouble(x1);
            xaxis.minimum = min;
            xaxis.maximum = max;
          } else {
            double max = xaxis.getDouble(x0);
            double min = xaxis.getDouble(x1);
            xaxis.minimum = min;
            xaxis.maximum = max;
          }

          if (y0 > y1) {
            double min = yaxis.getDouble(y0);
            double max = yaxis.getDouble(y1);
            yaxis.minimum = min;
            yaxis.maximum = max;
          } else {
            double min = yaxis.getDouble(y1);
            double max = yaxis.getDouble(y0);
            yaxis.minimum = min;
            yaxis.maximum = max;
          }

          update(getGraphics());
          repaint();
        }
      }
    }

    public void mouseClicked(MouseEvent e) {
      if (e.isAltDown() || (isLikeAltDown && e.isPopupTrigger())) {
        altDownMouseClicked(e);
      } else if (e.isPopupTrigger())
        showPopup(e);
      else if (e.getClickCount() == 2) {
        resetRange();

        userlimits = false;

        repaint();
      }
    }

    private void showPopup(MouseEvent e) {
      if (e.getComponent() instanceof Component) {

        popup = new JPopupMenu("Options");
        popup.setLightWeightPopupEnabled(false);

        JMenuItem mi = new JMenuItem("Scale...");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            if (range == null) range = new Range(G2Dint.this);

            range.setVisible(true);
            range.requestFocus();
            userlimits = true;
          }
        });
        popup.add(mi);

        mi = new JMenuItem("Reset scale");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            resetRange();
            userlimits = false;
            repaint();
          }
        });
        popup.add(mi);

        mi = new JMenuItem("Redraw");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            repaint();
          }
        });
        popup.add(mi);

        if (cpgin != null && cpgin.isVisible())
          mi = new JMenuItem("Hide coordinates");
        else
          mi = new JMenuItem("Show coordinates");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            if (cpgin == null) {
              cpgin = new Gin("Mouse position");
              cpgin.setLabels(x1, y1);
            }
            if (cpgin.isVisible())
              cpgin.setVisible(false);
            else
              cpgin.setVisible(true);
          }
        });
        popup.add(mi);

        if (dpgin != null && dpgin.isVisible())
          mi = new JMenuItem("Hide closest data point");
        else
          mi = new JMenuItem("Show closest data point");
        mi.addActionListener(new ActionListener() {
          public void actionPerformed(ActionEvent ae) {
            if (dpgin == null)
              dpgin = new Gin("Point coordinates");
            if (dpgin.isVisible())
              dpgin.setVisible(false);
            else {
              dpgin.setVisible(true);
              double d[] = getClosestPoint(x1, y1);
              dpgin.setLabels(d[0], d[1]);

              int ovalpointx = xaxis.getInteger(d[0]);
              int ovalpointy = yaxis.getInteger(d[1]);
              if (ovalpointx >= datarect.x && ovalpointx <= datarect.x + datarect.width &&
                      ovalpointy >= datarect.y && ovalpointy <= datarect.y + datarect.height) {
                getGraphics().fillOval(ovalpointx - 4, ovalpointy - 4, 8, 8);
              }
            }
          }
        });
        popup.add(mi);

	      if (ppgin != null && ppgin.isVisible())
		      mi = new JMenuItem("Hide peak info");
	      else
		      mi = new JMenuItem("Show peak info");
	      mi.addActionListener(new ActionListener() {
		      public void actionPerformed(ActionEvent ae) {
			      if (ppgin == null)
				      ppgin = new PeakInfoFrame(infoDefaultLength, plot_type);
			      if (ppgin.isVisible())
				      ppgin.setVisible(false);
			      else {
				      ppgin.setVisible(true);
				      PeakInfo[] peakInfos = getPeakInfoForCoordinate(x1);
				      ppgin.setLabels(peakInfos);
			      }
		      }
	      });
	      popup.add(mi);

        popup.show(e.getComponent(), e.getX(), e.getY());
      }
    }

  }

  class g2Dmousemotion extends MouseMotionAdapter {

    /**
     * Handle the Mouse Drag events
     */

    public void mouseDragged(MouseEvent e) {

      if (e.isAltDown() || (isLikeAltDown && e.isPopupTrigger())) {
        altDownMouseDragged(e);
      } else if ((xaxis != null && yaxis != null) && (popup == null || !popup.isVisible())) {

        x1 = e.getX();
        y1 = e.getY();

        if (drag) {

          if (x1 < datarect.x)
            x1 = datarect.x;
          else if (x1 > datarect.x + datarect.width)
            x1 = datarect.x + datarect.width;

          if (y1 < datarect.y)
            y1 = datarect.y;
          else if (y1 > datarect.y + datarect.height)
            y1 = datarect.y + datarect.height;

          update(getGraphics());

          if (cpgin != null && cpgin.isVisible())
            cpgin.setLabels(xaxis.getDouble(x1), yaxis.getDouble(y1));

          return;

        }

        if (cpgin != null && cpgin.isVisible())
          cpgin.setLabels(xaxis.getDouble(x1), yaxis.getDouble(y1));

        repaint();

      }
    }

    /**
     * Handle the Mouse Mouve events
     */

    public void mouseMoved(MouseEvent e) {

      if (e.isAltDown() || (isLikeAltDown && e.isPopupTrigger())) {
        altDownMouseMoved(e);
      } else if (xaxis != null && yaxis != null) {

        x1 = e.getX();
        y1 = e.getY();

        if (cpgin != null && cpgin.isVisible())
          cpgin.setLabels(xaxis.getDouble(x1), yaxis.getDouble(y1));

	      if (ppgin != null && ppgin.isVisible()) {
		      PeakInfo[] peakInfos = G2Dint.this.getPeakInfoForCoordinate(xaxis.getDouble(x1));
		      ppgin.setLabels(peakInfos);
	      }
      }
    }
  }


  /**
   *      Popup a window to output data after a Graphics Input command
   *      the window contains the following
   *           X  value
   *           Y  value
   */

  class Gin extends JFrame {

    JLabel xlabel = null;
    JLabel ylabel = null;

    /**
     * Instantiate the class
     */
    public Gin() {

      Container pane = getContentPane();
      pane.setLayout(new GridLayout(2, 1));

      xlabel = new JLabel("-000.000000000000000");
      ylabel = new JLabel("-000.000000000000000");

      JPanel jp1 = new JPanel();
      jp1.setLayout(new FlowLayout());
      jp1.add(new JLabel("x:"));
      jp1.add(xlabel);
      pane.add(jp1);

      jp1 = new JPanel();
      jp1.setLayout(new FlowLayout());
      jp1.add(new JLabel("y:"));
      jp1.add(ylabel);
      pane.add(jp1);

      setTitle("Select range");

      pack();

    }

    /**
     * Instantiate the class.
     * @param title the title to use on the pop-window.
     */

    public Gin(String title) {
      this();
      if (title != null)
        setTitle(title);
    }

    /**
     *  Set the X value
     * @param d The value to set it
     */

    public void setXlabel(double d) {
      xlabel.setText(String.valueOf(d));
    }

    /**
     *  Set the Y value
     * @param d The value to set it
     */

    public void setYlabel(double d) {
      ylabel.setText(String.valueOf(d));
    }

    /**
     *  Set the both values
     * @param dx The X value to set
     * @param dy The Y value to set
     */

    public void setLabels(double dx, double dy) {
      xlabel.setText(String.valueOf(dx));
      ylabel.setText(String.valueOf(dy));
    }

  }

	class PeakInfoFrame extends JFrame {

		JLabel[] infoLabel = null;
		JLabel[] positionLabel = null;

		/**
		 * Instantiate the class
		 */
		public PeakInfoFrame(int size, int plotType) {

			Container pane = getContentPane();
			pane.setLayout(new GridLayout(0, 2));

			switch (plotType) {
				case 1:          // G2Dint.REFLECTIVITY_PLOT
				case 2:          // G2Dint.FLUORESCENCE_PLOT
					pane.add(new JLabel("Element Line Energy(KeV) Probability          "));
					break;
				default: {
					pane.add(new JLabel("Phase     (h k l)        d-space "));
				}
			}
			pane.add(new JLabel("    Position "));

			infoLabel = new JLabel[size];
			positionLabel = new JLabel[size];

			for (int i = 0; i < size; i++) {
				switch (plotType) {
					case 1:          // G2Dint.REFLECTIVITY_PLOT
					case 2:          // G2Dint.FLUORESCENCE_PLOT
						pane.add(infoLabel[i] = new JLabel("                                 "));
						break;
					default: {
						pane.add(infoLabel[i] = new JLabel("                                                                          "));
					}
				}
				pane.add(positionLabel[i] = new JLabel("+######.##########"));
			}
			setTitle("Closest peaks:");

			pack();

		}

		public void setLabels(PeakInfo[] infos) {
			if (infos == null)
				return;
			for (int i = 0; i < infoLabel.length; i++) {
				if (i < infos.length) {
					infoLabel[i].setText(infos[i].info);
					positionLabel[i].setText(Double.toString(infos[i].coordinate));
				} else {
					infoLabel[i].setText("");
					positionLabel[i].setText("");
				}
			}
		}

	}

	/**
   *    A  popup window for altering the range of the plot
   */

  class Range extends JFrame {

    JTextField xminText = null;
    JTextField xmaxText = null;
    JTextField yminText = null;
    JTextField ymaxText = null;

    public Range(Graph2D g) {

      final Graph2D g2d = g;

      Container pane = getContentPane();
      pane.setLayout(new BorderLayout());

      JPanel rangepane = new JPanel();
      rangepane.setLayout(new BorderLayout(6, 6));
      pane.add(rangepane, BorderLayout.CENTER);

      JPanel jp1 = new JPanel();
      jp1.setLayout(new GridLayout(0, 1, 6, 6));
      xminText = addRow(jp1, new JLabel("Xmin:"), xaxis.minimum);
      xmaxText = addRow(jp1, new JLabel("Xmax:"), xaxis.maximum);
      yminText = addRow(jp1, new JLabel("Ymin:"), yaxis.minimum);
      ymaxText = addRow(jp1, new JLabel("Ymax:"), yaxis.maximum);
      rangepane.add("Center", jp1);

      jp1 = new JPanel();
      jp1.setLayout(new FlowLayout(FlowLayout.RIGHT, 6, 6));
      JButton cancel = new JButton("Cancel");
      JButton reset = new JButton("Reset");
      JButton done = new JButton("Done");
      jp1.add(cancel);
      jp1.add(reset);
      jp1.add(done);
      rangepane.add("South", jp1);

      cancel.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {

          range.setVisible(false);
          range.dispose();
        }
      });

      reset.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {

          xaxis.resetRange();
          yaxis.resetRange();

          userlimits = false;

          range.setVisible(false);
          range.dispose();
          G2Dint.this.repaint();
        }
      });

      done.addActionListener(new ActionListener() {
        public void actionPerformed(ActionEvent e) {
          Double d;
          double txmin = xaxis.minimum;
          double txmax = xaxis.maximum;
          double tymin = yaxis.minimum;
          double tymax = yaxis.maximum;

          d = Double.valueOf(xminText.getText());
          if (d != null) txmin = d.doubleValue();
          d = Double.valueOf(xmaxText.getText());
          if (d != null) txmax = d.doubleValue();
          d = Double.valueOf(yminText.getText());
          if (d != null) tymin = d.doubleValue();
          d = Double.valueOf(ymaxText.getText());
          if (d != null) tymax = d.doubleValue();

          if (txmax > txmin && tymax > tymin) {
            xaxis.minimum = txmin;
            xaxis.maximum = txmax;
            yaxis.minimum = tymin;
            yaxis.maximum = tymax;
          }

          userlimits = true;

          range.setVisible(false);
          range.dispose();
          G2Dint.this.repaint();
        }
      });
      pack();

    }

    public JTextField addRow(JPanel panel, JLabel l1, double value) {

      JPanel jp = new JPanel();
      jp.setLayout(new FlowLayout(FlowLayout.CENTER, 6, 6));
      jp.add(l1);
      JTextField textfield = new JTextField(20);
      textfield.setText(String.valueOf(value));
      jp.add(textfield);
      panel.add(jp);
      return textfield;
    }

  }

}
