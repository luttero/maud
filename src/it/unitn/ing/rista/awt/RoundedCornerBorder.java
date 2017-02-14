/*
 * @(#)RoundedCornerBorder.java created Nov 13, 2004 Braila
 *
 * Copyright (c) 1996-2004 Luca Lutterotti All Rights Reserved.
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

import javax.swing.border.BevelBorder;
import java.awt.*;


/**
 * The RoundedCornerBorder is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.1 $, $Date: 2004/11/18 09:38:50 $
 * @since JDK1.1
 */

public class RoundedCornerBorder extends BevelBorder {


  /**
   * Creates a bevel border with the specified type and whose
   * colors will be derived from the background color of the
   * component passed into the paintBorder method.
   *
   * @param bevelType the type of bevel for the border
   */
  public RoundedCornerBorder(int bevelType) {
    super(bevelType);
  }

  /**
   * Creates a bevel border with the specified type, highlight and
   * shadow colors.
   *
   * @param bevelType the type of bevel for the border
   * @param highlight the color to use for the bevel highlight
   * @param shadow    the color to use for the bevel shadow
   */
  public RoundedCornerBorder(int bevelType, Color highlight, Color shadow) {
    super(bevelType, highlight, shadow);
  }

  /**
   * Creates a bevel border with the specified type, highlight
   * shadow colors.
   *
   * @param bevelType           the type of bevel for the border
   * @param highlightOuterColor the color to use for the bevel outer highlight
   * @param highlightInnerColor the color to use for the bevel inner highlight
   * @param shadowOuterColor    the color to use for the bevel outer shadow
   * @param shadowInnerColor    the color to use for the bevel inner shadow
   */
  public RoundedCornerBorder(int bevelType, Color highlightOuterColor,
                             Color highlightInnerColor, Color shadowOuterColor,
                             Color shadowInnerColor) {
    super(bevelType, highlightOuterColor, highlightInnerColor,
        shadowOuterColor, shadowInnerColor);
  }

  /**
   * Paints the border for the specified component with the specified
   * position and size.
   *
   * @param c      the component for which this border is being painted
   * @param g      the paint graphics
   * @param x      the x position of the painted border
   * @param y      the y position of the painted border
   * @param width  the width of the painted border
   * @param height the height of the painted border
   */
  public void paintBorder(Component c, Graphics g, int x, int y, int width, int height) {
    Color oldColor = g.getColor();
    g.translate(x, y);

/*    if (bevelType == RAISED) {
      g.setColor(getHighlightOuterColor(c));
      g.drawLine(0, 0, width - 2, 0);
      g.drawLine(0, 0, 0, height - 2);
      g.drawLine(1, 1, 1, 1);

      g.setColor(getHighlightInnerColor(c));
      g.drawLine(2, 1, width - 2, 1);
      g.drawLine(1, 2, 1, height - 2);
      g.drawLine(2, 2, 2, 2);
      g.drawLine(0, height - 1, 0, height - 2);
      g.drawLine(width - 1, 0, width - 1, 0);

      g.setColor(getShadowOuterColor(c));
      g.drawLine(2, height - 1, width - 1, height - 1);
      g.drawLine(width - 1, 2, width - 1, height - 1);

      g.setColor(getShadowInnerColor(c));
      g.drawLine(width - 2, height - 2, width - 2, height - 2);


    } else if (bevelType == LOWERED) {
      g.setColor(getShadowOuterColor(c));
      g.drawLine(0, 0, width - 4, 0);
      g.drawLine(0, 0, 0, height - 4);
      g.drawLine(1, 1, 1, 1);

      g.setColor(getShadowInnerColor(c));
      g.drawLine(4, 1, width - 4, 1);
      g.drawLine(1, 4, 1, height - 4);
      g.drawLine(4, 4, 4, 4);
      g.drawLine(0, height - 1, 0, height - 4);
      g.drawLine(width - 1, 0, width - 1, 0);

      g.setColor(getHighlightOuterColor(c));
      g.drawLine(4, height - 1, width - 1, height - 1);
      g.drawLine(width - 1, 4, width - 1, height - 1);

      g.setColor(getHighlightInnerColor(c));
      g.drawLine(width - 4, height - 4, width - 4, height - 4);
    }*/
    g.translate(-x, -y);
    g.setColor(oldColor);
  }

  /**
   * Returns the insets of the border.
   *
   * @param c the component for which this border insets value applies
   */
  public Insets getBorderInsets(Component c) {
    return getBorderInsets(c, new Insets(0, 0, 0, 0));
  }

  /**
   * Reinitialize the insets parameter with this Border's current Insets.
   *
   * @param c      the component for which this border insets value applies
   * @param insets the object to be reinitialized
   */
  public Insets getBorderInsets(Component c, Insets insets) {
    insets.top = insets.left = insets.bottom = insets.right = 12;
    return insets;
  }

  /**
   * Returns whether or not the border is opaque.
   */
  public boolean isBorderOpaque() {
    return true;
  }

}
