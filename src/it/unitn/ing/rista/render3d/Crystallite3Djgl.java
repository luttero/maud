/*
 * @(#)Crystallite3Djgl.java created 08/01/2001 Mesiano
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

import it.unitn.ing.rista.awt.*;
import it.unitn.ing.rista.interfaces.*;
import it.unitn.ing.rista.util.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.lang.*;
import java.util.*;

import jgl.*;

/**
 * The Crystallite3Djgl is a class
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:09 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Crystallite3Djgl extends AnimatedRendering3Djgl {

  public double scaleplot = 1.0;

  public Crystallite3Djgl(Shape3D crystalliteShape, int drawmode,
                          int resolution, double scaleplot) {
    super("Shape", crystalliteShape, drawmode, resolution);

    this.scaleplot = scaleplot / 100.0;
  }


  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public void defineShape(int mode, int slices) {

    float rho, drho, theta, dtheta;
    double radius;
    float x, y, z;
    float s, t, ds, dt;
    int i, j, imin, imax;
    boolean normals;
    float nsign;
    double xvect[] = null;

    normals = true;
    nsign = 1.0f;

    int stacks = slices;

    drho = (float) (Constants.PI / stacks);
    dtheta = (float) (2.0 * Constants.PI / slices);

    Shape3D crystalliteShape = (Shape3D) objectToRender;

    float cryst_color[] = new float[4];
    cryst_color[0] = (float) Constants.crystallite_color.getRed() / 255;
    cryst_color[1] = (float) Constants.crystallite_color.getGreen() / 255;
    cryst_color[2] = (float) Constants.crystallite_color.getBlue() / 255;
    cryst_color[3] = 1.0f;

    gl.glMaterialfv(gl.GL_FRONT, gl.GL_SPECULAR, Constants.crystallite_mat_specular);
    gl.glMaterialfv(gl.GL_FRONT, gl.GL_SHININESS, Constants.crystallite_mat_shininess);
    gl.glMaterialfv(gl.GL_FRONT, gl.GL_AMBIENT_AND_DIFFUSE, cryst_color);

/* texturing: s goes from 0.0/0.25/0.5/0.75/1.0 at +y/+x/-y/-x/+y axis */
/* t goes from -1.0/+1.0 at z = -radius/+radius (linear along longitudes) */
/* cannot use triangle fan on texturing (s coord. at top/bottom tip varies) */

    gl.glColor3f(1.0f, 1.0f, 1.0f);
    gl.glBegin(gl.GL_LINES);
    gl.glVertex3f(0f, 0f, 0f);
    radius = crystalliteShape.getNormalizedShapeR(0.0, Constants.PI / 2.0) * scaleplot;
    gl.glVertex3f((float) (1.5 * radius), 0f, 0f);
    gl.glEnd();

    gl.glColor3f(1.0f, 1.0f, 1.0f);
    gl.glBegin(gl.GL_LINES);
    gl.glVertex3f(0f, 0f, 0f);
    radius = crystalliteShape.getNormalizedShapeR(Constants.PI / 2.0,
            Constants.PI / 2.0) * scaleplot;
    gl.glVertex3f(0f, (float) (1.5 * radius), 0f);
    gl.glEnd();

    gl.glColor3f(1.0f, 1.0f, 1.0f);
    gl.glBegin(gl.GL_LINES);
    gl.glVertex3f(0f, 0f, 0f);
    radius = crystalliteShape.getNormalizedShapeR(0.0, 0.0) * scaleplot;
    gl.glVertex3f(0f, 0f, (float) (1.5 * radius));
    gl.glEnd();

    gl.glColor4fv(cryst_color);
    if (mode == 1) {
/* draw +Z end as a triangle fan */

      gl.glBegin(gl.GL_TRIANGLE_FAN);
      gl.glNormal3f(0.0f, 0.0f, 1.0f);
//      gl.glTexCoord2f(0.5f,1.0f);
      radius = crystalliteShape.getNormalizedShapeR(0.0, 0.0) * scaleplot;
      gl.glVertex3f(0.0f, 0.0f, (float) (nsign * radius));
      for (j = 0; j <= slices; j++) {
        if (j == slices)
          theta = 0.0f;
        else
          theta = (float) (j * dtheta);
        x = (float) (-Math.sin((float) theta) * Math.sin((float) drho));
        y = (float) (Math.cos((float) theta) * Math.sin((float) drho));
        z = (float) (nsign * Math.cos((float) drho));
        if (normals)
          gl.glNormal3f((float) (x * nsign), (float) (y * nsign), (float) (z * nsign));

        radius = crystalliteShape.getNormalizedShapeR(theta, drho) * scaleplot;
        gl.glVertex3f((float) (x * radius), (float) (y * radius), (float) (z * radius));
      }
      gl.glEnd();

      ds = 1.0f / slices;
      dt = 1.0f / stacks;
      t = 1.0f;  /* because loop now runs from 0 */
      imin = 0;
      imax = stacks;

/* draw intermediate stacks as quad strips */
      for (i = imin; i < imax; i++) {
        rho = i * drho;
        gl.glBegin(gl.GL_QUAD_STRIP);
        s = 0.0f;
        for (j = 0; j <= slices; j++) {
          if (j == slices)
            theta = 0.0f;
          else
            theta = (float) (j * dtheta);
          x = (float) (-Math.sin(theta) * Math.sin(rho));
          y = (float) (Math.cos(theta) * Math.sin(rho));
          z = (float) (nsign * Math.cos(rho));
          if (normals)
            gl.glNormal3f((float) (x * nsign), (float) (y * nsign), (float) (z * nsign));
//	   			gl.glTexCoord2f(s,t);
          radius = crystalliteShape.getNormalizedShapeR(theta, rho) * scaleplot;
          gl.glVertex3f((float) (x * radius), (float) (y * radius), (float) (z * radius));
          x = (float) (-Math.sin(theta) * Math.sin(rho + drho));
          y = (float) (Math.cos(theta) * Math.sin(rho + drho));
          z = (float) (nsign * Math.cos(rho + drho));
          if (normals)
            gl.glNormal3f((float) (x * nsign), (float) (y * nsign), (float) (z * nsign));
//	    		gl.glTexCoord2f(s,t-dt);
          s += ds;
          radius = crystalliteShape.getNormalizedShapeR(theta, rho + drho) * scaleplot;
          gl.glVertex3f((float) (x * radius), (float) (y * radius), (float) (z * radius));
        }
        gl.glEnd();
        t -= dt;
      }

/* draw -Z end as a triangle fan */
      gl.glBegin(gl.GL_TRIANGLE_FAN);
      gl.glNormal3f(0.0f, 0.0f, -1.0f);
//      gl.glTexCoord2f(0.5f,0.0f);
      radius = crystalliteShape.getNormalizedShapeR(0.0, 0.0) * scaleplot;
      gl.glVertex3f(0.0f, 0.0f, (float) (-radius * nsign));
      rho = (float) (Constants.PI - drho);
      s = 1.0f;
      t = dt;
      for (j = slices; j >= 0; j--) {
        if (j == slices)
          theta = 0.0f;
        else
          theta = (float) (j * dtheta);
        x = (float) (-Math.sin(theta) * Math.sin(rho));
        y = (float) (Math.cos(theta) * Math.sin(rho));
        z = (float) (nsign * Math.cos(rho));
        if (normals)
          gl.glNormal3f((float) (x * nsign), (float) (y * nsign), (float) (z * nsign));
//	 			gl.glTexCoord2f(s,t);
        s -= ds;
        radius = crystalliteShape.getNormalizedShapeR(theta, rho) * scaleplot;
        gl.glVertex3f((float) (x * radius), (float) (y * radius), (float) (z * radius));
      }
      gl.glEnd();
    } else if (mode == 2) {//(qobj->DrawStyle==GLU_LINE || qobj->DrawStyle==GLU_SILHOUETTE) {
/* draw stack lines */

      for (i = 1; i < stacks; i++) {  /* stack line at i==stacks-1 was missing here */
        rho = i * drho;
        gl.glBegin(gl.GL_LINE_LOOP);
        for (j = 0; j < slices; j++) {
          theta = j * dtheta;
          x = (float) (Math.cos(theta) * Math.sin(rho));
          y = (float) (Math.sin(theta) * Math.sin(rho));
          z = (float) (Math.cos(rho));
          if (normals)
            gl.glNormal3f((float) (x * nsign), (float) (y * nsign), (float) (z * nsign));
          radius = crystalliteShape.getNormalizedShapeR(theta, rho) * scaleplot;
          gl.glVertex3f((float) (x * radius), (float) (y * radius), (float) (z * radius));
        }
        gl.glEnd();
      }
/* draw slice lines */
      for (j = 0; j < slices; j++) {
        theta = j * dtheta;
        gl.glBegin(gl.GL_LINE_STRIP);
        for (i = 0; i <= stacks; i++) {
          rho = i * drho;
          x = (float) (Math.cos(theta) * Math.sin(rho));
          y = (float) (Math.sin(theta) * Math.sin(rho));
          z = (float) (Math.cos(rho));
          if (normals)
            gl.glNormal3f((float) (x * nsign), (float) (y * nsign), (float) (z * nsign));
          radius = crystalliteShape.getNormalizedShapeR(theta, rho) * scaleplot;
          gl.glVertex3f((float) (x * radius), (float) (y * radius), (float) (z * radius));
        }
        gl.glEnd();
      }
    } else if (mode == 3) {//(qobj->DrawStyle==GLU_POINT) {
/* top and bottom-most points */

      gl.glBegin(gl.GL_POINTS);
      if (normals)
        gl.glNormal3f(0.0f, 0.0f, nsign);
      radius = crystalliteShape.getNormalizedShapeR(0.0, 0.0) * scaleplot;
      gl.glVertex3d(0.0f, 0.0f, radius);
      if (normals)
        gl.glNormal3f(0.0f, 0.0f, -nsign);
      gl.glVertex3d(0.0f, 0.0f, -radius);

/* loop over stacks */
      for (i = 1; i < stacks - 1; i++) {
        rho = i * drho;
        for (j = 0; j < slices; j++) {
          theta = j * dtheta;
          x = (float) (Math.cos(theta) * Math.sin(rho));
          y = (float) (Math.sin(theta) * Math.sin(rho));
          z = (float) (Math.cos(rho));
          if (normals)
            gl.glNormal3f((float) (x * nsign), (float) (y * nsign), (float) (z * nsign));
          radius = crystalliteShape.getNormalizedShapeR(theta, rho) * scaleplot;
          gl.glVertex3f((float) (x * radius), (float) (y * radius), (float) (z * radius));
        }
      }
      gl.glEnd();
    }

  }

}
