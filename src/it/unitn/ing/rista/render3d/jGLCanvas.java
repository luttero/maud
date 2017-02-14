/*
 * @(#)jGLCanvas.java created 08/01/2001 Mesiano
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

package it.unitn.ing.rista.render3d;

import jgl.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * The jGLCanvas is a class
 *
 * @version $Revision: 1.8 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */


public class jGLCanvas extends JComponent implements ComponentListener {

  public GL gl = new GL();
  protected GLU glu = new GLU(gl);
  GLAUX myAUX = new GLAUX(gl);
  Dimension size = new Dimension(0, 0);

  protected boolean mustResize = false;

  protected boolean cvsInit = false;

  protected long _f_dur = 0;

  /**
   *
   * Constructor
   *
   * Uses the default GLFunc and GLUFunc implementation !
   *
   * @param width    	the canvas initial-prefered width
   * @param height   	the canvas initial-prefered height
   *
   */
  public jGLCanvas(int width, int height) {
    super();

    size = new Dimension(width, height);

    setSize(size);

  }

  /* GLCanvas AWT classes */

  public Dimension getPreferredSize() {
    return getMinimumSize();
  }

  public Dimension getMinimumSize() {
    return size;
  }

  /**
   *
   * Overridden update
   * This one only call's the paint method, without clearing
   * the background - thats hopefully done by OpenGL ;-)
   *
   * @param g		the Graphics Context
   * @return 		void
   *
   * @see	jGLCanvas#paint
   */
/*  public void update(Graphics g) {

    paint(g);
  }  */

  /**
   * this function overrides the Canvas paint method !
   *
   */
  public void paint(Graphics g) {
    super.paint(g);
    sDisplay(g);
//			System.out.println("painting");
  }

  /**
   *
   * This is the thread save rendering-method called by paint.
   * The actual thread will be set to highes priority befor calling
   * 'display'. After 'display' the priority will be reset !
   *
   * 'gljFree' will be NOT called after 'display'.
   *
   * We tested the above to use multi-threading and
   * for the demonstration 'glDemos' it works ;-)) !
   *
   * BE SURE, if you want to call 'display' by yourself
   * (e.g. in the run method for animation)
   * YOU HAVE TO CALL sDisplay  -- OR YOU MUST KNOW WHAT YOU ARE DOING THEN !
   *
   *
   * @see jGLCanvas#paint
   * @see jGLCanvas#display
   */
  public synchronized final void sDisplay(Graphics g) {
    boolean ok = true;

    long _s = System.currentTimeMillis();

    if (mustResize) {
      size = getSize();
      reshape(size.width, size.height);
      mustResize = false;
      invalidate();
      repaint(100);
    }

//			System.out.println("painting " + cvsInit);
    if (cvsInit) {
      display();
      gl.glXSwapBuffers(g, null);  // null to avoid notify
    }

    _f_dur = System.currentTimeMillis() - _s;
  }

  public void componentMoved(ComponentEvent e) {
  }

  public void componentShown(ComponentEvent e) {
  }

  public void componentHidden(ComponentEvent e) {
  }

  public void componentResized(ComponentEvent e) {
    if (e.getComponent() == this) {
      mustResize = true;
      repaint();
    }
  }

  protected void reshape(int w, int h) {
  }

  public void display() {
  }

  public void init() {

    initToDisplayOnly();
  }

  public void initToDisplayOnly() {

    myAUX.auxInitPosition(0, 0, getSize().width, getSize().height);
    myAUX.auxInitWindow(this);

    addComponentListener(this);

    initGL();

    reshape(getSize().width, getSize().height);

    cvsInit = true;

    repaint();
  }

  protected void initGL() {
  }

  public void stop() {
  }

}

