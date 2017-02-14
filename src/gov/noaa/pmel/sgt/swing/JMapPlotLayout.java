/*
 * @(#)JMapPlotLayout.java created Jun 1, 2003 Berkeley
 *
 * Copyright (c) 1996-2003 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with the author.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package gov.noaa.pmel.sgt.swing;

import gov.noaa.pmel.util.Point2D;
import gov.noaa.pmel.sgt.SGLabel;

import java.awt.*;


/**
 * The JMapPlotLayout is a class
 *
 * @version $Revision: 1.2 $, $Date: 2004/08/12 09:36:02 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JMapPlotLayout extends JPlotLayout {

  public JMapPlotLayout(boolean isGrid, boolean isXTime,
                        boolean isYTime, String id, Image img,
                        boolean is_key_pane) {
    super(isGrid, isXTime, isYTime, id, img, is_key_pane);
    Point2D.Double pt = new Point2D.Double(0, 0);
    setAxesOriginP(pt);
  }

  /**
   * Set the layer size in physical units
   */

  public void setTitlePosition(double x, double y) {
    mainTitle_.setLocationP(new Point2D.Double(x, y));
  }

  public SGLabel getTitle() {
    return mainTitle_;
  }
}
