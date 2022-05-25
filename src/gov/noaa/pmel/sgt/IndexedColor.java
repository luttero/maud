/*
 * $Id: IndexedColor.java,v 1.1 2004/12/27 16:15:19 luca Exp $
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

import java.awt.Color;

/**
 * Defines the access methods for color maps that support indexed color.
 *
 * @author Donald Denbo
 * @version $Revision: 1.1 $, $Date: 2004/12/27 16:15:19 $
 * @since 2.0
 **/
public interface IndexedColor {

  /**
   *
   * @param index
   * @return
   * @since 3.0
   */
  Color getColorByIndex(int index);

  void setColor(int index, Color color);

  void setColor(int index, int red, int green, int blue);
  /**
   * Get the maximum legal value of the color index.
   */
  int getMaximumIndex();
}
