/*
 * $Id: Moveable.java,v 1.1 2004/12/27 16:15:19 luca Exp $
 *
 * This software is provided by NOAA for full, free and open release.  It is
 * understood by the recipient/user that NOAA assumes no liability for any
 * errors contained in the code.  Although this software is released without
 * conditions or restrictions in its use, it is expected that appropriate
 * credit be given to its author and to the National Oceanic and Atmospheric
 * Administration should the software be included by the recipient as an
 * element in other product development.
 */
 
package gov.noaa.pmel.sgt;

import java.awt.Point;
import java.awt.Rectangle;
import java.beans.PropertyChangeListener;
 
/**
 * Interface indicates that object can be moved with a mouse drag.
 * Objects are notified of movement via the PropertyChange mechanism.
 * 
 * @author Donald Denbo
 * @version $Revision: 1.1 $, $Date: 2004/12/27 16:15:19 $
 * @since 2.0
 */
public interface Moveable {
  /**
   * Gets the bounding rectangle in device coordinates.
   *
   * @since 2.0
   * @return bounding rectangle
   */
  public Rectangle getBounds();
  /**
   * Gets the location in device coordinates.
   *
   * @since 2.0
   * @return location
   */
  public Point getLocation();
  /**
   * Sets the location in device coordinates.
   * @since 2.0
   *
   */
  public void setLocation(Point point);
  /**
   * Returns true if the current state is moveable
   *
   * @since 2.0
   * @return true if moveable
   */
  public boolean isMoveable();
  /**
   * Set the moveable property.
   *
   * @since 2.0
   * @param select if true object is moveable
   */
  public void setMoveable(boolean move);
  /**
   * Add a new PropertyChangeListener.  Properties will include
   * "moved".  Implementation of the following two methods will
   * normally be via the PropertyChangeSupport class.
   * @since 2.0
   */
  public void addPropertyChangeListener(PropertyChangeListener l);
  /**
   * Remove a listener.
   * @since 2.0
   */
  public void removePropertyChangeListener(PropertyChangeListener l);
}
