/*
 * @(#)JPlotSpectraLayout.java created Jul 20, 2003 $Berkeley
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
 * The JPlotSpectraLayout is a class
 *  
 * @version $Revision: 1.3 $, $Date: 2004/12/27 16:05:16 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class JPlotSpectraLayout extends JPlotLayout {

  public JPlotSpectraLayout(boolean isGrid, boolean isXTime, boolean isYTime,
                            boolean isLinearXAxis,
                            boolean isLinearYAxis,
                            String id, Image img,
                            boolean is_key_pane) {
    super(isGrid, isXTime, isYTime, isLinearXAxis, isLinearYAxis, id, img, is_key_pane);
//    keyHeight_ = KEY_HEIGHT_ = 0.20;
  }

}
