/*
 * @(#)MaudProgressBar.java created Aug 24, 2003 Braila
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

import javax.swing.*;
import javax.swing.plaf.ColorUIResource;
import java.awt.*;


/**
 * The MaudProgressBar is a class
 *  
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:52 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class MaudProgressBar extends JPanel {

  private int currentValue = 0;
  private int minValue = 0;
  private int maxValue = 100;
  private Color darkblue = new Color(0, 0, 255);
  private Color paleblue = new Color(200, 200, 255);
  private int orientation = JProgressBar.HORIZONTAL;

  private static final Color white = new Color(255, 255, 255); //#FFFFFF
  private static final Color black = new Color(0, 0, 0); //#000000

  private static final Color gray0 = new Color(238, 238, 238); //#EEEEEE
  private static final Color gray1 = new Color(221, 221, 221); //#DDDDDD
  private static final Color gray2 = new Color(204, 204, 204); //#CCCCCC
  private static final Color gray3 = new Color(187, 187, 187); //#BBBBBB
  private static final Color gray4 = new Color(170, 170, 170); //#AAAAAA
  private static final Color gray5 = new Color(153, 153, 153); //#999999
  private static final Color gray6 = new Color(136, 136, 136); //#888888
  private static final Color gray7 = new Color(119, 119, 119); //#777777
  private static final Color gray8 = new Color(107, 107, 107); //#666666
  private static final Color gray9 = new Color(85, 85, 85); //#555555
  private static final Color gray10 = new Color(68, 68, 68); //#444444
  private static final Color gray11 = new Color(34, 34, 34); //#333333
  private static final Color gray12 = new Color(17, 17, 17); //#222222
  private static final Color gray13 = new Color(51, 51, 51); // Luca
  private static final Color gray99 = new Color(99, 99, 99);

  // Patch to support theme colors...

/* Lavender */
  private static Color accent = new Color(102, 102, 204); //#6666CC *** clut-3
  private static Color accentHighlight = new Color(153, 153, 255); //#9999FF *** clut-2
  private static Color accentLightHighlight = new Color(204, 204, 255); //#CCCCFF *** clut-1
  private static Color accentShadow = new Color(51, 51, 153); //#333399 *** menu highlight clut-4 (zero-based)
  private static Color accentDarkShadow = new Color(0, 0, 85); //#000055 *** clut-6

/*
  private static Color accent = new Color(102, 204, 102); //#6666CC *** clut-3
  private static Color accentHighlight = new Color(153, 255, 153); //#9999FF *** clut-2
  private static Color accentLightHighlight = new Color(204, 255, 204); //#CCCCFF *** clut-1
  private static Color accentShadow = new Color(51, 153, 51); //#333399 *** menu highlight clut-4 (zero-based)
  private static Color accentDarkShadow = new Color(0, 85, 0); //#000055 *** clut-6
*/

/*
  private static Color accent = new Color(204, 102, 102); //#6666CC *** clut-3
  private static Color accentHighlight = new Color(255, 153, 153); //#9999FF *** clut-2
  private static Color accentLightHighlight = new Color(255, 204, 204); //#CCCCFF *** clut-1
  private static Color accentShadow = new Color(153, 51, 51); //#333399 *** menu highlight clut-4 (zero-based)
  private static Color accentDarkShadow = new Color(85, 0, 0); //#000055 *** clut-6
*/

  public MaudProgressBar(int min, int max) {
    this();
    minValue = min;
    maxValue = max;
  }

  public MaudProgressBar() {
    setPreferredSize(new Dimension(200, 12));
  }

/*  public void paint(Graphics g) {
    Graphics2D g2 = (Graphics2D) g;
    Dimension s = getSize();
    int height = s.height;
    int width = s.width;
    g2.setPaint(darkblue);
    int fraction = (currentValue * width) / (maxValue - minValue);
    g2.fill(new Rectangle(0, 0, fraction, height));
    g2.setPaint(paleblue);
    g2.fill(new Rectangle(fraction, 0, width - fraction, height));
  }*/

  /**
   * This determines the amount of the progress bar that should be filled
   * based on the percent done gathered from the model. This is a common
   * operation so it was abstracted out. It assumes that your progress bar
   * is linear. That is, if you are making a circular progress indicator,
   * you will want to override this method.
   */
  protected int getAmountFull(Insets b, int width, int height) {
    int amountFull = 0;

    if ((getMaximum() - getMinimum()) != 0) {
      if (getOrientation() == JProgressBar.HORIZONTAL) {
//        System.out.println(width + " " + getPercentComplete());
        amountFull = (int) Math.round(getSize().width *
                getPercentComplete());
      } else {
        amountFull = (int) Math.round(getSize().height *
                getPercentComplete());
      }
    }
    return amountFull;
  }

  private int getMaximum() {
    return maxValue;
  }

  private int getMinimum() {
    return minValue;
  }

  private double getPercentComplete() {
    return ((double) currentValue) / (maxValue - minValue);
  }

  public void paint(Graphics g /*, JComponent c */) {
//    BoundedRangeModel model = getModel();

    super.paint(g);
    int barRectX = 1;
    int barRectY = 1;
    int barRectWidth = getWidth() - 1;
    int barRectHeight = getHeight() - 1;
    Insets b = getInsets(); // area for border
    barRectX += b.left;
    barRectY += b.top;
    barRectWidth -= (b.right + barRectX);
    barRectHeight -= (b.bottom + barRectY);

    int current;
    // a cell and its spacing
    int increment = getCellLength() + getCellSpacing();
    // amount of progress to draw
    int amountFull = getAmountFull(b, barRectWidth, barRectHeight);

    g.setColor(getForeground());
    if (getOrientation() == JProgressBar.HORIZONTAL) {
      //Black Border
      g.setColor(getBlack());
      g.drawRect(barRectX - 1, barRectY - 1,
              barRectWidth + 1, barRectHeight + 1);

      // draw the cells
//	  if (getCellSpacing() == 0 && amountFull > 0) {
      // Highlighting
      //     over the unfilled portion
      //     well, draw all the way across; let others draw over it
      g.setColor(getGray3());
      g.fillRect(barRectX + amountFull, barRectY,
              barRectWidth - 1 - amountFull, barRectHeight - 1);

      //     line on left
      g.setColor(getGray9());
      g.drawLine(barRectX + amountFull + 1, barRectY,
              barRectX + amountFull + 1, barRectHeight + barRectY - 1);
      //     line on left
      g.setColor(getGray6());
      g.drawLine(barRectX + amountFull + 2, barRectY,
              barRectX + amountFull + 2, barRectHeight + barRectY - 1);

      //     line on right
      g.setColor(getGray1());
      g.drawLine(barRectWidth + barRectX - 1, barRectY,
              barRectWidth + barRectX - 1, barRectHeight);

      //     line on top
      g.setColor(getGray6());
      g.drawLine(barRectX + amountFull + 3, barRectY,
              barRectWidth + barRectX - 1, barRectY);

      //     line on bottom
      g.setColor(getGray1());
      g.drawLine(barRectX + amountFull + 3, barRectHeight + barRectY - 1,
              barRectWidth + barRectX - 1, barRectHeight + barRectY - 1);

      // draw one big Rect because there is no space between cells
      g.setColor(getAccent());
      g.fillRect(barRectX, barRectY,
              barRectX - b.left + amountFull, barRectHeight);
      if (amountFull - b.left > 5) {
        g.setColor(getBlack());

        g.drawLine(barRectX + amountFull, barRectY,
                barRectX + amountFull, barRectHeight + barRectY - 1);


        g.setColor(getAccentDarkShadow());

        g.drawLine(barRectX + 2, barRectHeight + barRectY - 1,
                barRectX + amountFull - 1, barRectHeight + barRectY - 1);

        g.drawLine(barRectX + amountFull - 1, barRectY + 1,
                barRectX + amountFull - 1, barRectHeight + barRectY - 2);


        g.setColor(getAccentShadow());
        g.drawLine(barRectX + 2, barRectY,
                barRectX + amountFull - 1, barRectY);

        g.drawLine(barRectX + 2, barRectHeight + barRectY - 2,
                barRectX + amountFull - 2, barRectHeight + barRectY - 2);

        g.drawLine(barRectX + amountFull - 2, barRectY + 2,
                barRectX + amountFull - 2, barRectHeight + barRectY - 3);

        g.drawLine(barRectX + 1, barRectHeight + barRectY - 1,
                barRectX + 1, barRectHeight + barRectY - 1);


        g.setColor(getAccentHighlight());
        g.drawLine(barRectX + 2, barRectY + 2,
                barRectX + amountFull - 3, barRectY + 2);

        g.drawLine(barRectX + 2, barRectHeight + barRectY - 4,
                barRectX + amountFull - 3, barRectHeight + barRectY - 4);

        g.drawLine(barRectX + 1, barRectHeight + barRectY - 3,
                barRectX + 1, barRectHeight + barRectY - 3);
        g.drawLine(barRectX + 1, barRectY + 1,
                barRectX + 1, barRectY + 1);


        g.setColor(getAccentLightHighlight());
        g.drawLine(barRectX + 2, barRectY + 3,
                barRectX + amountFull - 3, barRectY + 3);

        g.drawLine(barRectX + 2, barRectHeight + barRectY - 5,
                barRectX + amountFull - 3, barRectHeight + barRectY - 5);

        g.drawLine(barRectX + amountFull - 3, barRectY + 4,
                barRectX + amountFull - 3, barRectHeight + barRectY - 6);

        g.drawLine(barRectX + 1, barRectHeight + barRectY - 4,
                barRectX + 1, barRectHeight + barRectY - 4);
        g.drawLine(barRectX + 1, barRectY + 2,
                barRectX + 1, barRectY + 2);

        g.setColor(getWhite());
        g.fillRect(barRectX + 2, barRectY + 4,
                barRectX - b.left + amountFull - 5, barRectHeight - 9);
        g.drawLine(barRectX + 1, barRectY + 3,
                barRectX + 1, barRectHeight + barRectY - 5);
      }
//	    } else { // draw each individual cells
      // the largest number to draw a cell at
//		int max = barRectX + amountFull;

/*		for (current = barRectX;
             current < max;
             current += increment) {
        g.setColor(getAccent());
            g.fillRect(current, barRectY,
                   getCellLength(), barRectHeight);
        }*/
//	    }
    } else { // VERTICAL

      //Black Border
      g.setColor(getBlack());
      g.drawRect(barRectX - 1, barRectY - 1,
              barRectWidth + 1, barRectHeight + 1);

      // draw the cells
//	  if (getCellSpacing() == 0 && amountFull > 0) {
      // Highlighting
      //     over the unfilled portion
      //     well, draw all the way across; let others draw over it
      g.setColor(getGray3());
      g.fillRect(barRectX, barRectY + amountFull,
              barRectWidth - 1, barRectHeight - 1 - amountFull);

      //     line on left
      g.setColor(getGray9());
      g.drawLine(barRectX, barRectY + amountFull + 1,
              barRectX + barRectWidth - 1, barRectY + amountFull + 1);
      //     line on left
      g.setColor(getGray6());
      g.drawLine(barRectX, barRectY + amountFull + 2,
              barRectX + barRectWidth - 1, barRectY + amountFull + 2);

      //     line on right
      g.setColor(getGray1());
      g.drawLine(barRectWidth + barRectX - 1, barRectY,
              barRectWidth + barRectX - 1, barRectHeight);

      //     line on top
      g.setColor(getGray6());
      g.drawLine(barRectX, barRectY + amountFull + 3,
              barRectWidth + barRectX - 1, barRectY + amountFull + 3);

      //     line on bottom
      g.setColor(getGray1());
      g.drawLine(barRectX, barRectHeight + barRectY - 1,
              barRectWidth + barRectX - 1, barRectHeight + barRectY - 1);

      // draw one big Rect because there is no space between cells
      g.setColor(getAccent());
      g.fillRect(barRectX, barRectY,
              barRectWidth, barRectHeight - b.top + amountFull);
      if (amountFull - b.top > 5) {
        g.setColor(getBlack());

        g.drawLine(barRectX, barRectY + amountFull,
                barRectWidth + barRectX - 1, barRectY + amountFull);


        g.setColor(getAccentDarkShadow());

        g.drawLine(barRectWidth + barRectX - 1, barRectY + 2, barRectWidth + barRectX - 1, barRectY
                + amountFull - 1);

        g.drawLine(barRectX + 1, barRectY + amountFull - 1, barRectHeight + barRectX - 2, barRectY +
                amountFull - 1);


        g.setColor(getAccentShadow());

        g.drawLine(barRectX, barRectY + 2, barRectX, barRectY + amountFull - 1);

        g.drawLine(barRectWidth + barRectX - 2, barRectY + 2, barRectWidth + barRectX - 2, barRectY
                + amountFull - 2);

        g.drawLine(barRectX + 2, barRectY + amountFull - 2, barRectWidth + barRectX - 3, barRectY + amountFull - 2);

        g.drawLine(barRectWidth + barRectX - 1, barRectY + 1, barRectWidth + barRectX - 1, barRectY
                + 1);


        g.setColor(getAccentHighlight());

        g.drawLine(barRectX + 2, barRectY + 2, barRectX + 2, barRectY + amountFull - 3);

        g.drawLine(barRectWidth + barRectX - 4, barRectY + 2, barRectWidth + barRectX - 4, barRectY
                + amountFull - 3);

        g.drawLine(barRectWidth + barRectX - 3, barRectY + 1, barRectWidth + barRectX - 3, barRectY
                + 1);

        g.drawLine(barRectX + 1, barRectY + 1, barRectX + 1, barRectY + 1);


        g.setColor(getAccentLightHighlight());

        g.drawLine(barRectX + 3, barRectY + 2, barRectX + 3, barRectY + amountFull - 3);
        g.drawLine(barRectWidth + barRectX - 5, barRectY + 2, barRectWidth + barRectX - 5,
                barRectY + amountFull - 3);

        g.drawLine(barRectX + 4, barRectY + amountFull - 3, barRectWidth + barRectX - 6, barRectY + amountFull - 3);

        g.drawLine(barRectWidth + barRectX - 4, barRectY + 1, barRectWidth + barRectX - 4, barRectY
                + 1);

        g.drawLine(barRectX + 2, barRectY + 1, barRectX + 2, barRectY + 1);

        g.setColor(getWhite());
        g.fillRect(barRectX + 4, barRectY + 2,
                barRectWidth - 9, barRectX - b.left + amountFull - 5);

        g.drawLine(barRectX + 3, barRectY + 1, barRectWidth + barRectX - 5,
                barRectY + 1);
      }


    }

    // Deal with possible text painting
/*    if (progressBar.isStringPainted()) {
      paintString(g, barRectX, barRectY,
              barRectWidth, barRectHeight,
              amountFull, b);
    }*/
  }

  private int getCellSpacing() {
    return 0;
  }

  private int getCellLength() {
    return 1;
  }

  static final Color getWhite() {
    return white;
  }

  static final Color getBlack() {
    return black;
  }

  // note - 0 is lightest, 10 is darkest

  /** #EEEEEE - very light accents */
  static final Color getGray0() {
    return gray0;
  }

  /** #DDDDDD - main control color, background */
  static final Color getGray1() {
    return gray1;
  }

  /** #CCCCCC - scrollbar track highlight */
  static final Color getGray2() {
    return gray2;
  }

  /** #BBBBBB - scrollbar track highlight */
  static final Color getGray3() {
    return gray3;
  }

  /** #AAAAAA - control shadow, button shadow */
  static final Color getGray4() {
    return gray4;
  }

  /** #999999 - window frame shadow */
  static final Color getGray5() {
    return gray5;
  }

  /** #888888 - scrollbar track shadow */
  static final Color getGray6() {
    return gray6;
  }

  /** #777777 - control dark shadow, button dark shadow */
  static final Color getGray7() {
    return gray7;
  }

  /** #666666 - inactive text color */
  static final Color getGray8() {
    return gray8;
  }

  /** #555555 - pressed button shadow */
  static final Color getGray9() {
    return gray9;
  }

  /** #444444 - pressed button dark shadow */
  static final Color getGray10() {
    return gray10;
  }

  /** #333333 - pressed button dark shadow */
  static final Color getGray11() {
    return gray11;
  }

  /** #222222 - pressed button dark shadow */
  static final Color getGray12() {
    return gray12;
  }

  /** Luca added - tabPanel dark shadow */
  static final Color getGray13() {
    return gray13;
  }

// Additional color by David Himelright
  static final Color getGray99() {
    return gray99;
  }


  /** #6666CC - for Lavender accents*/
  static final Color getAccent() {
    return accent;
  }

  /** #9999FF - for Lavender accents*/
  static final Color getAccentHighlight() {
    return accentHighlight;
  }

  /** #CCCCFF - for Lavender accents*/
  static final Color getAccentLightHighlight() {
    return accentLightHighlight;
  }

  /** #333399 - for Lavender accents*/
  static final Color getAccentShadow() {
    return accentShadow;
  }

  /** #000055 - for Lavender accents*/
  static final Color getAccentDarkShadow() {
    return accentDarkShadow;
  }

  private int getOrientation() {
    return orientation;  //To change body of created methods use Options | File Templates.
  }

  public void setValue(int val) {
    if ((val >= minValue) && (val <= maxValue)) {
      currentValue = val;
      repaint();
    }
  }

  public void setMinimum(int value) {
    minValue = value;
  }

  public void setMaximum(int value) {
    maxValue = value;
  }

}
