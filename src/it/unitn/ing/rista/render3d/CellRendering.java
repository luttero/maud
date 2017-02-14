/*
 * @(#)CellRendering.java created 18/12/1998 Pergine
 *
 * Copyright (c) 1998 Luca Lutterotti All Rights Reserved.
 *
 * This software is the research result of Luca Lutterotti and it is
 * provided as it is as confidential and proprietary information.
 * You shall not disclose such Confidential Information and shall use
 * it only in accordance with the terms of the license agreement you
 * entered into with Luca Lutterotti.
 *
 * THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
 * SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
 * SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
 * THIS SOFTWARE OR ITS DERIVATIVES.
 *
 */

package it.unitn.ing.rista.render3d;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.awt.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

/**
 *  The representation of a Crystal structure.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:09 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class CellRendering extends JComponent implements Runnable {

  XYZChemModel md;
  boolean painted = true;
  float xfac;
  int prevx, prevy;
  float xtheta, ytheta;
  float scalefudge = .9f;
  Matrix3D amat = new Matrix3D(), tmat = new Matrix3D();
  String message = null;
  Image backBuffer;
  Graphics backGC;
  Dimension backSize;
  Phase thephase;
  int mode = 0;
//  Frame theparent = null;

  public CellRendering(Frame parent, Phase phase, int mode) {
    super();
    thephase = phase;
    this.mode = mode;
//    theparent = parent;
  }

  private synchronized void newBackBuffer() {
    backBuffer = createImage(getSize().width, getSize().height);
    if (backBuffer != null)
      backGC = backBuffer.getGraphics();
    backSize = getSize();
    addMouseListener(new CRmouse());
    addMouseMotionListener(new CRmousemotion());
  }

  public void run() {
    amat.yrot(20);
    amat.xrot(20);
    setSize(getSize().width <= 20 ? 400 : getSize().width,
            getSize().height <= 20 ? 400 : getSize().height);
    newBackBuffer();

    try {
      Thread.currentThread().setPriority(Thread.MIN_PRIORITY);
      XYZChemModel m = new XYZChemModel();
      m.buildtheModel(thephase, this, mode);
      md = m;
      m.findBB();
      float xw = (float) (m.xmax - m.xmin);
      float yw = (float) (m.ymax - m.ymin);
      float zw = (float) (m.zmax - m.zmin);
      if (yw > xw)
        xw = yw;
      if (zw > xw)
        xw = zw;
      float f1 = (float) getSize().width / xw;
      float f2 = (float) getSize().height / xw;
      xfac = 0.7f * (f1 < f2 ? f1 : f2) * scalefudge;
      setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
    } catch (Exception e) {
      e.printStackTrace();
      md = null;
      message = e.toString();
    }
    repaint();
  }

  public void start() {
    if (md == null && message == null)
      new Thread(this).start();
  }

  public void stop() {
  }

  public void update(Graphics g) {
    if (backBuffer == null)
      g.clearRect(0, 0, getSize().width, getSize().height);
    paintComponent(g);
  }

  public void paintComponent(Graphics g) {
    if (md != null) {
      md.mat.unit();
      md.mat.translate(-(md.xmin + md.xmax) / 2,
              -(md.ymin + md.ymax) / 2,
              -(md.zmin + md.zmax) / 2);
      md.mat.mult(amat);
// md.mat.scale(xfac, -xfac, 8 * xfac / getSize().width);
      md.mat.scale(xfac, -xfac, 16 * xfac / getSize().width);
      md.mat.translate(getSize().width / 2, getSize().height / 2, 8);
      md.transformed = false;
      if (backBuffer != null) {
        if (!backSize.equals(getSize()))
          newBackBuffer();
        backGC.setColor(getBackground());
        backGC.fillRect(0, 0, getSize().width, getSize().height);
        md.paint(backGC);
        g.drawImage(backBuffer, 0, 0, this);
      } else
        md.paint(g);
      setPainted();
    } else if (message != null) {
      g.drawString("Error in model:", 3, 20);
      g.drawString(message, 10, 40);
    }
  }

  private synchronized void setPainted() {
    painted = true;
    notifyAll();
  }

  private synchronized void waitPainted() {
    while (!painted) {
      try {
        wait();
      } catch (InterruptedException e) {
      }
    }
    painted = false;
  }

  class CRmouse extends MouseAdapter {

    /**
     * Handle the Mouse Down events
     */

    public void mousePressed(MouseEvent e) {

      int x = e.getX();
      int y = e.getY();

      prevx = x;
      prevy = y;
      setCursor(new Cursor(Cursor.MOVE_CURSOR));
    }

    /**
     * Handle the Mouse Up events
     */

    public void mouseReleased(MouseEvent e) {
      setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));
    }

    public void mouseClicked(MouseEvent e) {
    }

  }

  class CRmousemotion extends MouseMotionAdapter {

    /**
     * Handle the Mouse Drag events
     */

    public void mouseDragged(MouseEvent e) {

      int x = e.getX();
      int y = e.getY();

      tmat.unit();
      float xtheta = (prevy - y) * (360.0f / getSize().width);
      float ytheta = (x - prevx) * (360.0f / getSize().height);
      tmat.xrot(xtheta);
      tmat.yrot(ytheta);
      amat.mult(tmat);
      if (painted) {
        painted = false;
        update(getGraphics());
//		repaint();
      }
      prevx = x;
      prevy = y;
    }

    /**
     * Handle the Mouse Mouve events
     */

    public void mouseMoved(MouseEvent e) {

    }
  }

}

