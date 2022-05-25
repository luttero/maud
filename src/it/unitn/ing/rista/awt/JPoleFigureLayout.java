/*
 * @(#)JPoleFigureLayout.java created Jun 1, 2003 Berkeley
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

package it.unitn.ing.rista.awt;

import gov.noaa.pmel.sgt.swing.JPlotLayout;
import gov.noaa.pmel.sgt.*;
import gov.noaa.pmel.sgt.LinearTransform;
import gov.noaa.pmel.util.*;

import java.awt.*;


/**
 * The JPoleFigureLayout is a class
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:03 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JPoleFigureLayout extends JPlotLayout {

  public JPoleFigureLayout(boolean isGrid, boolean isXTime, boolean isYTime,
                           boolean isLinearXAxis,
                           boolean isLinearYAxis,
                           String id, Image img,
                           boolean is_key_pane) {
    super(isGrid, isXTime, isYTime, isLinearXAxis, isLinearYAxis, id, img, is_key_pane);
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

/*	public void setLayerSizeP(Dimension2D d) {
    Component[] comps = getComponents();
    CartesianGraph graph = (CartesianGraph) getFirstLayer().getGraph();
    LinearTransform yt = (LinearTransform) graph.getYTransform();
    LinearTransform xt = (LinearTransform) graph.getXTransform();
    for (int i = 0; i < comps.length; i++) {
      if (comps[i] instanceof Layer) {
        ((Layer) comps[i]).setSizeP(d);
      }
    }
    yt.setRangeP(new Range2D(0, 6));
    xt.setRangeP(new Range2D(0, 6));
    //
    mainTitle_.setLocationP(new Point2D.Double(-100, -100));
    title2_.setLocationP(new Point2D.Double(-100, -100));
    title3_.setLocationP(new Point2D.Double(-100, -100));
  }*/
}

/*
  public JMapPlotLayout(boolean isGrid, boolean isXTime,
                        boolean isYTime, String id, Image img,
                        boolean is_key_pane) {
    super(isGrid, isXTime, isYTime, id, img, is_key_pane);
    Point2D.Double pt = new Point2D.Double(0, 0);
    setAxesOriginP(pt);
  }

	public void setTitlePosition(double x, double y) {
		mainTitle_.setLocationP(new Point2D.Double(x, y));
	}

	public SGLabel getTitle() {
		return mainTitle_;
	}

*/