/*
 * @(#)AnimatedRendering3Djgl.java created 13/01/2001 Casalino
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

import it.unitn.ing.rista.interfaces.i3DCanvas;
import it.unitn.ing.rista.util.Constants;
import jgl.GL;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;

/**
 * The AnimatedRendering3Djgl is a class
 *
 * @version $Revision: 1.10 $, $Date: 2006/01/19 14:45:58 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class AnimatedRendering3Djgl extends jGLAnimCanvas implements i3DCanvas, MouseMotionListener//, MouseListener
{

  protected int startMousePositionX = 0,
  startMousePositionY = 0,
  deltaMousePositionX = 0,
  deltaMousePositionY = 0;

  protected int x = 0,
  y = 0;

  protected float startView_rotx = 0.0f,
  startView_roty = 0.0f,
  startView_rotz = 0.0f,
  endView_rotx = startView_rotx,
  endView_roty = startView_roty,
  endView_rotz = startView_rotz;

  protected float addView_rotx = 0.0f,
  addView_roty = 0.0f,
  addView_rotz = 1.0f;

  protected boolean isDragging = false;
  public boolean isRotating = false;
  protected int resolution;
  protected int drawmode;
  public float scaleplot = 1.0f;

  protected Object objectToRender = null;
  protected int objectID = 1;

// these are differents ----------------------------------------------------

  public AnimatedRendering3Djgl(String title, Object obj, int drawmode, int resolution,
                                int width, int height) {

    super(width, height);

    setName(title);

    setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));

//	  addMouseListener(this);
    addMouseMotionListener(this);

    this.resolution = resolution;
    objectToRender = obj;
    this.drawmode = drawmode;
  }

  public AnimatedRendering3Djgl(String title, Object obj, int drawmode, int resolution) {

    this(title, obj, drawmode, resolution, Constants.kSceneWidth, Constants.kSceneHeight);

  }

  public void initDisplay() {
  }

  public void closeDisplay() {

    gl.glFlush();

  }

  public void init() {
		isRotating = true;
    super.init();
  }

  public synchronized void stop() {

    isRotating = false;
    super.stop();
  }

// up to here ---------------------------------------------------------------

  public void update() {
    stop();
    super.init();
    repaint();
  }

  public void zoomIn() {
    scaleplot /= 1.2;
    boolean wasRotating = isRotating;
    stop();
    if (wasRotating)
      start();
    else
      update();
  }

  public void zoomOut() {
    scaleplot *= 1.2;
    boolean wasRotating = isRotating;
    stop();
    if (wasRotating)
      start();
    else
      update();
  }

  public void reshape(int w, int h) {
    gl.glViewport(0, 0, w, h);		/* define the viewport */
    gl.glMatrixMode(GL.GL_PROJECTION);
    gl.glLoadIdentity();
    glu.gluPerspective(/* field of view in degrees */ 40.0,
            /* aspect ratio */ 1.0,
            /* Z near */ 1.0, /* Z far */ 10.0);
    gl.glMatrixMode(GL.GL_MODELVIEW);
    gl.glLoadIdentity();
    glu.gluLookAt(5.0, 0.0, 0.0, // eye is at (5,0,0)
            0.0, 0.0, 0.0, // center is at (0,0,0)
            0.0, 0.0, 1.0); 				// up is in +Z direction

    gl.glTranslatef(-1.0f, 0.0f, 0.0f);
  }

  protected void initGL() {

    gl.glClearColor(Constants.openglBackColor[0], Constants.openglBackColor[1], Constants.openglBackColor[2],
            Constants.openglBackColor[3]);
//		gl.glShadeModel(gl.GL_SMOOTH);
//		gl.glPolygonMode(gl.GL_FRONT, gl.GL_FILL);
    gl.glEnable(GL.GL_DEPTH_TEST);
    gl.glDepthFunc(GL.GL_LESS);

    gl.glColorMaterial(GL.GL_FRONT, GL.GL_AMBIENT_AND_DIFFUSE);
    gl.glEnable(GL.GL_COLOR_MATERIAL);

    gl.glLightfv(GL.GL_LIGHT0, GL.GL_POSITION, Constants.lightpos);
    gl.glEnable(GL.GL_LIGHTING);
    gl.glEnable(GL.GL_LIGHT0);

    gl.glNewList(objectID, GL.GL_COMPILE); /* create display list */
    defineShape(drawmode, /* slices */ resolution);
    gl.glEndList();
  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  public void display() {

//			System.out.println("displaying");
    initDisplay();

    gl.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);

    gl.glPushMatrix();

    gl.glRotatef(startView_rotx, 1.0f, 0.0f, 0.0f);
    gl.glRotatef(startView_roty, 0.0f, 1.0f, 0.0f);
    gl.glRotatef(startView_rotz, 0.0f, 0.0f, 1.0f);

    gl.glCallList(objectID);
    gl.glPopMatrix();

    if (!isDragging && isRotating) {
      startView_rotx += addView_rotx;
      startView_roty += addView_roty;
      startView_rotz += addView_rotz;
    }

    // preventing errors
    while (startView_rotz < 0.0)
      startView_rotz += 360.0;
    while (startView_rotz > 360.0)
      startView_rotz -= 360.0;
    while (startView_rotx < 0.0)
      startView_rotx += 360.0;
    while (startView_rotx > 360.0)
      startView_rotx -= 360.0;
    while (startView_roty < 0.0)
      startView_roty += 360.0;
    while (startView_roty > 360.0)
      startView_roty -= 360.0;

    closeDisplay();

  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public void defineShape(int mode, int slices) {
    // to be overwrited by subclasses
  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public void mouseClicked(MouseEvent evt) {
  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public void mouseEntered(MouseEvent evt) {
  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public void mouseExited(MouseEvent evt) {
  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public void mousePressed(MouseEvent evt) {

    isDragging = true;

    setCursor(new Cursor(Cursor.MOVE_CURSOR));

    x = evt.getX();
    y = evt.getY();

    startMousePositionX = x;
    startMousePositionY = y;
    deltaMousePositionX = 0;
    deltaMousePositionY = 0;

    endView_rotx = startView_rotx;
    endView_roty = startView_roty;
    endView_rotz = startView_rotz;
  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public void mouseReleased(MouseEvent evt) {

    setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));

    isDragging = false;

    repaint();
  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public void mouseDragged(MouseEvent evt) {
    x = evt.getX();
    y = evt.getY();

    deltaMousePositionX = -x + startMousePositionX;	// left negative, right positive
    deltaMousePositionY = -y + startMousePositionY;	// down negative, up positive

    startView_rotz = endView_rotz - deltaMousePositionX;
    startView_roty = endView_roty - deltaMousePositionY;
//		startView_rotx = endView_rotx;

    repaint();
  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public void mouseMoved(MouseEvent evt) {
    x = evt.getX();
    y = evt.getY();

  }

}
