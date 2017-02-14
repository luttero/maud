/*
 * @(#)MapRendering3Dgl.java created Aug 15, 2004 Braila
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

package it.unitn.ing.rista.render3d;

import it.unitn.ing.jgraph.ThermalColorMap;
import it.unitn.ing.rista.diffr.Reflection;
import it.unitn.ing.rista.util.MoreMath;
import it.unitn.ing.rista.util.Constants;

import javax.media.opengl.*;
import javax.media.opengl.glu.GLUquadric;
import javax.media.opengl.glu.GLU;
import java.awt.*;
import java.nio.FloatBuffer;


/**
 * The MapRendering3Dgl is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/01/19 14:45:59 $
 * @since JDK1.1
 */

public class MapRendering3Dgl extends AnimatedRendering3Dgl {

  private int alphaSlices;
  private int betaSlices;
  private int gammaSlices;
  public boolean logScale = false;
  public int colrsNumber = 64;
  public ThermalColorMap thermalMap = null;
  private double[][][] odf;
  int mode = 0;

  public MapRendering3Dgl(double[][][] odf, int alphaSlices, int betaSlices, int gammaSlices,
                          int drawmode, int resolution, boolean logScale, int colrsNumber)
//, GraphicsConfiguration g, GLCapabilities glCaps)
  {

    super(null, drawmode, resolution);
    this.odf = odf;
    this.alphaSlices = alphaSlices;
    this.betaSlices = betaSlices;
    this.gammaSlices = gammaSlices;
//, g, glCaps);

/*    String log = "";
    if (logScale)
      log = " (log scale)";
    setName(first + log);*/
    this.logScale = logScale;
    this.colrsNumber = colrsNumber;

  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public void defineShape(GL2 gl, int model, int slices) {

/* texturing: s goes from 0.0/0.25/0.5/0.75/1.0 at +y/+x/-y/-x/+y axis */
/* t goes from -1.0/+1.0 at z = -radius/+radius (linear along longitudes) */
/* cannot use triangle fan on texturing (s coord. at top/bottom tip varies) */

    float zmax = 0.0f;
    float zmin = 0.0f;
    mode = model;

    for (int i = 0; i < alphaSlices; i++)
      for (int j = 0; j < betaSlices; j++)
        for (int n = 0; n < gammaSlices; n++) {
        if (logScale && !Double.isNaN(odf[i][j][n]))
          odf[i][j][n] = MoreMath.log10(odf[i][j][n]);
//      		PF[i][j] *= scaleStrain;
//      		PF[i][j] += random;
        if (!Double.isNaN(odf[i][j][n])) {
          if (zmax < odf[i][j][n])
            zmax = (float) odf[i][j][n];
          if (zmin > odf[i][j][n])
            zmin = (float) odf[i][j][n];
        }
      }

    float offset = 0.0f;
    if (zmin < 0)
      offset = -zmin;
    for (int i = 0; i < alphaSlices; i++)
      for (int j = 0; j < betaSlices; j++)
        for (int n = 0; n < gammaSlices; n++) {
        odf[i][j][n] += offset;
//        System.out.println(PF[i][j]);
      }
    zmax += offset;
    zmin += offset;

    thermalMap = new ThermalColorMap(zmin, zmax, colrsNumber, Constants.grayShaded);

    drawStructure(gl);
  }

  public void drawStructure(GL2 gl) {

/* texturing: s goes from 0.0/0.25/0.5/0.75/1.0 at +y/+x/-y/-x/+y axis */
/* t goes from -1.0/+1.0 at z = -radius/+radius (linear along longitudes) */
/* cannot use triangle fan on texturing (s coord. at top/bottom tip varies) */

    float halfPosAlpha = alphaSlices / 2;
    float halfPosBeta = betaSlices / 2;
    float halfPosGamma = gammaSlices / 2;
    float x1, y1, z1;

    if (mode == 0) {
      gl.glEnable (GL.GL_BLEND);
      gl.glDepthMask (false);
      gl.glBlendFunc (GL.GL_SRC_ALPHA, GL.GL_ONE);
      gl.glMaterialfv(GL.GL_FRONT, GL2.GL_EMISSION, FloatBuffer.wrap(mat_emission));
      gl.glMaterialfv(GL.GL_FRONT, GL2.GL_DIFFUSE, FloatBuffer.wrap(mat_transparent));
      gl.glPointSize(5.0f);
      gl.glEnable(GL2.GL_POINT_SMOOTH);
      gl.glBegin(GL.GL_POINTS);
      for (int i = 0; i < alphaSlices; i++) {
        x1 = (i - halfPosAlpha) / 5.0f;
        for (int j = 0; j < betaSlices; j++) {
          y1 = (j - halfPosBeta) / 5.0f;
          for (int n = 0; n < gammaSlices; n++) {
            z1 = (n - halfPosGamma) / 5.0f;
//            float[] colorDensity = defineColorByIntensity((float) odf[i][j][n]);
//            gl.glColor3f(colorDensity[0], colorDensity[1], colorDensity[2]);
            gl.glColor4fv(FloatBuffer.wrap(defineColorByIntensity((float) odf[i][j][n])));
            gl.glVertex3f(x1 / scaleplot, y1 / scaleplot, z1 / scaleplot);
//            gl.glColor4fv(defineColorByIntensity((float) odf[i][j][n]));
          }
        }
      }
//      gl.glEnd();
    } else {
//(qobj->DrawStyle==GLU_POINT) {
/* top and bottom-most points */

/* loop over stacks */
//      gl.glBegin();
//      gl.glTranslatef(-halfPosAlpha, -halfPosBeta, -halfPosGamma);
      gl.glEnable (GL.GL_BLEND);
      gl.glDepthMask (false);
      gl.glBlendFunc (GL.GL_SRC_ALPHA, GL.GL_ONE);
      gl.glMaterialfv(GL.GL_FRONT, GL2.GL_EMISSION, FloatBuffer.wrap(mat_emission));
      gl.glMaterialfv(GL.GL_FRONT, GL2.GL_DIFFUSE, FloatBuffer.wrap(mat_transparent));
      for (int i = 0; i < alphaSlices; i++) {
        x1 = (i - halfPosAlpha) / 5.0f;
        for (int j = 0; j < betaSlices; j++) {
          y1 = (j - halfPosBeta) / 5.0f;
          for (int n = 0; n < gammaSlices; n++) {
            z1 = (n - halfPosGamma) / 5.0f;
            float[] colorDensity = defineColorByIntensity((float) odf[i][j][n]);
            gl.glColor3f(colorDensity[0], colorDensity[1], colorDensity[2]);
//            gl.glVertex3f(x1, y1, z1);
            gl.glTranslatef(x1 / scaleplot, y1 / scaleplot, z1 / scaleplot);
            texture(gl, (float) odf[i][j][n]);
//            gl.glMaterialfv(GL.GL_FRONT, GL.GL_AMBIENT_AND_DIFFUSE, defineColorByIntensity((float) odf[i][j][n]));
            gl.glColor4fv(FloatBuffer.wrap(colorDensity));
            GLUquadric quadObj = glu.gluNewQuadric();
            glu.gluQuadricDrawStyle(quadObj, GLU.GLU_FILL);
            glu.gluQuadricNormals(quadObj, GLU.GLU_SMOOTH);
            glu.gluSphere(quadObj, halfPosAlpha / scaleplot, 16, 16);

//            glut.glutSolidCube(0.1f / scaleplot);
            gl.glTranslatef(-x1 / scaleplot, -y1 / scaleplot, -z1 / scaleplot);

          }
        }
      }
//      gl.glEnd();
    }

  }

//  float[] mat_zero = { 0.0f, 0.0f, 0.0f, 1.0f };
  float[] mat_transparent = { 0.0f, 0.8f, 0.8f, 0.6f };
  float[] mat_emission = { 0.0f, 0.3f, 0.3f, 0.6f };

  public void texture(GL2 gl, float z) {
/*    float[] mat_col = defineColorByIntensity(z);
    float[] mat_color = new float[4];
    float[] mat_shin = new float[1];
    for (int i = 0; i < 3; i++)
      mat_color[i] = mat_col[i] / 2.5f;
    mat_color[3] = mat_col[3];*/
    gl.glMaterialfv(GL.GL_FRONT, GL2.GL_EMISSION, FloatBuffer.wrap(mat_emission));
    gl.glMaterialfv(GL.GL_FRONT, GL2.GL_DIFFUSE, FloatBuffer.wrap(mat_transparent));
//    gl.glMaterialfv(GL.GL_FRONT, GL.GL_DIFFUSE, mat_col);
//    gl.glMaterialfv(GL.GL_FRONT, GL.GL_SPECULAR, mat_color);
//    gl.glMaterialfv(GL.GL_FRONT, GL.GL_SHININESS, mat_shin);
  }

  public void textureNotVisible(GL2 gl) {
    float[] mat_color = new float[4];
    mat_color[0] = 0;
    mat_color[1] = 0;
    mat_color[2] = 0;
    mat_color[3] = 0;
    float[] mat_shin = new float[1];
    mat_shin[0] = 0;
    gl.glMaterialfv(GL.GL_FRONT, GL2.GL_AMBIENT_AND_DIFFUSE, FloatBuffer.wrap(mat_color));
    gl.glMaterialfv(GL.GL_FRONT, GL2.GL_SPECULAR, FloatBuffer.wrap(mat_color));
    gl.glMaterialfv(GL.GL_FRONT, GL2.GL_SHININESS, FloatBuffer.wrap(mat_shin));
  }

  public float[] defineColorByIntensity(float z) {
    Color tmcolor = thermalMap.getColor(z);
    float[] mat_color = new float[4];
    mat_color[0] = tmcolor.getRed() / 255f;
    mat_color[1] = tmcolor.getGreen() / 255f;
    mat_color[2] = tmcolor.getBlue() / 255f;
    mat_color[3] = 0.6f;
//    mat_color[4] = 0.1f;
    return mat_color;
  }

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

    drawStructure(gl);

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

}
