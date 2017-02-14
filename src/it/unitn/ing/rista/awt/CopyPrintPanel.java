/*
 * @(#)CopyPrintPanel.java created Feb 27, 2003 Berkeley
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
import java.awt.*;
import java.awt.print.PageFormat;

/**
 * The CopyPrintPanel is a class
 *  
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:52 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class CopyPrintPanel extends JComponent {

  PageFormat pageFormat = null;

  public void clearComponent(Graphics g) {
    clearComponent(g, this);
  }

  public static void clearComponent(Graphics g, Component comp) {
    g.clearRect(0, 0, comp.getWidth(), comp.getHeight());
//    System.out.println(comp + " " + comp.getWidth() + " " + comp.getHeight());
    if (comp instanceof Container) {
      Component[] compList = ((Container) comp).getComponents();
      for (int i = 0; i < compList.length; i++) {
        clearComponent(g, compList[i]);
      }
    }
  }

/*  public void paintComponent(Graphics g) {
    paintComponent(g, this);
  }*/

  public static void paintComponent(Graphics g, Component comp) {
    g.translate(comp.getX(), comp.getY());
    comp.paint(g);
    g.translate(-comp.getX(), -comp.getY());
//    System.out.println(comp + " " + comp.getWidth() + " " + comp.getHeight());
    if (comp instanceof Container) {
      Component[] compList = ((Container) comp).getComponents();
      for (int i = 0; i < compList.length; i++) {
        paintComponent(g, compList[i]);
      }
    }
  }

  public void print(Graphics g, PageFormat pf) {
    pageFormat = pf;
    double pageHeight = pageFormat.getImageableHeight();
    double pageWidth = pageFormat.getImageableWidth();
    int orientation = pageFormat.getOrientation();
    double scaleX = pageWidth / getWidth();
    double scaleY = pageHeight / getHeight();
/*    if (orientation == PageFormat.PORTRAIT) {
      scaleX = pageHeight / getWidth();
      scaleY = pageWidth / getHeight();
    }*/
    double scale = (scaleY > scaleX) ? scaleX : scaleY;
    Graphics2D g2 = (Graphics2D) g;
    g2.scale(scale, scale);
    super.print(g);
    g2.scale(1.0/scale, 1.0/scale);
//    clearComponent(g, this);
//    paintComponent(g, this);
  }

}
