/*
 * @(#)AnimatedRendering3Dgl.java created 13/01/2001 Casalino
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

import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.interfaces.i3DCanvas;

import javax.media.opengl.*;
import javax.media.opengl.awt.*;
import javax.media.opengl.fixedfunc.*;
import javax.media.opengl.glu.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseListener;
import java.awt.*;
import java.nio.FloatBuffer;

import com.jogamp.opengl.util.gl2.GLUT;
import com.jogamp.opengl.util.Animator;

/**
 * The AnimatedRendering3Dgl is a class
 *
 * @version $Revision: 1.11 $, $Date: 2006/11/10 09:33:01 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class AnimatedRendering3Dgl implements i3DCanvas, GLEventListener, MouseListener, MouseMotionListener {

  protected int startMousePositionX = 0,
  startMousePositionY = 0,
  deltaMousePositionX = 0,
  deltaMousePositionY = 0;

  protected int x = 0, y = 0;

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
  protected boolean isRotating = false;
  public float scaleplot = 1.0f;

  protected int resolution;
  protected int drawmode;

  protected Object objectToRender = null;
  protected int objectID = 1;

//  protected GL gl;
  protected GLU glu = new GLU();
  protected GLUT glut = new GLUT();
//  protected GLAutoDrawable gldrawable;

  int width = Constants.kSceneWidth;
  int height = Constants.kSceneHeight;
  boolean needUpdating = true;
  myAnimator animator = null;


// these are differents ----------------------------------------------------

  public AnimatedRendering3Dgl(Object obj, int drawmode, int resolution, int width, int height) {

    objectToRender = obj;
    this.drawmode = drawmode;
    this.resolution = resolution;
    this.width = width;
    this.height = height;
  }

  public AnimatedRendering3Dgl(Object obj, int drawmode, int resolution) {

    this(obj, drawmode, resolution, Constants.kSceneWidth, Constants.kSceneHeight);

  }

  public void setWidth(int width) {
    this.width = width;
  }

  public int getWidth() {
    return width;
  }

  public void setHeight(int height) {
    this.height = height;
  }

  public int getHeight() {
    return height;
  }

  public void setSize(int width, int height) {
    this.width = width;
    this.height = height;
  }

  public void initDisplay() {
  }

  public void closeDisplay() {
  }

//  boolean initialized = false;

/*	public void init() {
		initialized = true;
		initGL(gl);
		reshape(getSize().width, getSize().height);
//		start();
	}*/

  public void setCanvas(GLCanvas canvas) {
    animator = new myAnimator(canvas);
  }

  public synchronized void start() {
    isRotating = true;
    if (animator != null && !animator.isAnimating())
        animator.start();
  }

  public synchronized void stop() {
    isRotating = false;
    if (animator != null && animator.isAnimating())
      animator.stop();
  }

// up to here ---------------------------------------------------------------

  public void update() {
    needUpdating = true;
//    boolean wasRotating = isRotating;
//    if (isRotating)
//		  stop();
//    initGL(gl);
//		reshape(getSize().width, getSize().height);
    if (!isRotating)
      repaint();
//    if (wasRotating)
//      start();
  }

  public void zoomIn() {
    scaleplot /= 1.2;
//    boolean wasRotating = isRotating;
//    stop();
//    if (wasRotating)
//      start();
//    else
    update();
  }

  public void zoomOut() {
    scaleplot *= 1.2;
/*    boolean wasRotating = isRotating;
    stop();
    if (wasRotating)
      start();
    else        */
    update();
  }

  public void repaint() {
    display();
  }

  public void display() {
    if (!isRotating) {
      animator.display();
/*      if (animator != null) {
        animator.start();
        try {
          Thread.sleep(10);
        } catch (InterruptedException e) {}
        animator.stop();
      }*/
    }
//    display(gldrawable);
  }

  protected void initGL(GL2 gl) {

//    System.out.println("start");
    gl.glClearColor(Constants.openglBackColor[0], Constants.openglBackColor[1],
      Constants.openglBackColor[2], Constants.openglBackColor[3]);
//    System.out.println("clear done");
//		gl.glShadeModel(gl.GL_SMOOTH);
//		gl.glPolygonMode(gl.GL_FRONT, gl.GL_FILL);
//    System.out.println("depth");
    gl.glEnable(GL.GL_DEPTH_TEST);
    gl.glDepthFunc(GL.GL_LESS);

//    System.out.println("color");
    gl.glColorMaterial(GL.GL_FRONT, GL2.GL_AMBIENT_AND_DIFFUSE);
    gl.glEnable(GL2.GL_COLOR_MATERIAL);

//    System.out.println("light");
    gl.glLightfv(GL2.GL_LIGHT0, GL2.GL_POSITION, FloatBuffer.wrap(Constants.lightpos));
    gl.glEnable(GL2.GL_LIGHTING);
    gl.glEnable(GL2.GL_LIGHT0);

//    System.out.println("newlist");
    gl.glNewList(objectID, GL2.GL_COMPILE); /* create display list */
//    System.out.println("shape");
    defineShape(gl, drawmode, /* slices */ resolution);
//    System.out.println("endlist");
    gl.glEndList();
//    gl.glEnable(GL.GL_NORMALIZE); // new one

//    System.out.println("end");
  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  public void display(GLAutoDrawable drawable) {
//    gldrawable = drawable;
    GL2 gl = drawable.getGL().getGL2();
    if (needUpdating) {
      initGL(gl);
      needUpdating = false;
    }
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
//    System.out.println("Rotating " + startView_rotz);

    closeDisplay();

  }

  public void displayChanged(GLAutoDrawable drawable, boolean modeChanged, boolean deviceChanged) {
//    gldrawable = drawable;
  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public void defineShape(GL gl, int mode, int slices) {
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

//		setCursor(new Cursor(Cursor.MOVE_CURSOR));

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

//		setCursor(new Cursor(Cursor.CROSSHAIR_CURSOR));

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
//		x = evt.getX();
//		y = evt.getY();

  }

  public void init(GLAutoDrawable drawable) {
//    gldrawable = drawable;
    GL2 gl = drawable.getGL().getGL2();
//    glu = new GLU();
//    this.gldrawable = drawable;
//    System.err.println("INIT GL IS: " + gl.getClass().getName());

    initGL(gl);
    ((Component) drawable).addMouseListener(this);
    ((Component) drawable).addMouseMotionListener(this);
  }

  public void dispose(GLAutoDrawable glAutoDrawable) {
    
  }

  public void reshape(GLAutoDrawable drawable, int x, int y, int width, int height) {
//    gldrawable = drawable;
    GL2 gl = drawable.getGL().getGL2();
    gl.glViewport(0, 0, width, height);		/* define the viewport */
//    float h = (float)height / (float)width; //in gears
    gl.glMatrixMode(GL2.GL_PROJECTION);
//    if (!gl.isFunctionAvailable("glLoadTransposeMatrixfARB")) {
      // --- not using extensions
      gl.glLoadIdentity();
//    } else {
      // --- using extensions
//      final float[] identityTranspose = new float[] {
//        1, 0, 0, 0,
//        0, 1, 0, 0,
//        0, 0, 1, 0,
//        0, 0, 0, 1
//      };
//      gl.glLoadTransposeMatrixfARB(identityTranspose);
//    }
//    gl.glFrustum(-1.0f, 1.0f, -h, h, 5.0f, 60.0f); //in gears
    glu.gluPerspective(40.0, // field of view in degrees
        1.0, // aspect ratio
        1.0, 10.0); // Z near, Z far
    gl.glMatrixMode(GL2.GL_MODELVIEW);
    gl.glLoadIdentity();
    glu.gluLookAt(5.0, 0.0, 0.0, // eye is at (5,0,0)
            0.0, 0.0, 0.0, // center is at (0,0,0)
            0.0, 0.0, 1.0); 				// up is in +Z direction

    gl.glTranslatef(-1.0f, 0.0f, 0.0f);
//    System.out.println("reshape");
  }

  class myAnimator extends Animator {

    public myAnimator(GLCanvas canvas) {
      super(canvas);
    }

    public void display() {
      super.display();
    }
  }

}
