/*
 * @(#)PoleRendering3Dgl.java created 29/01/1999 Mesiano
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

import it.unitn.ing.jgraph.ThermalColorMap;
import it.unitn.ing.rista.diffr.Reflection;
import it.unitn.ing.rista.util.Constants;
import it.unitn.ing.rista.util.MoreMath;

import javax.media.opengl.*;
import java.awt.*;
import java.nio.FloatBuffer;

/**
 * The PoleRendering3Dgl is a class for 3D rendering of the pole figure.
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.4 $, $Date: 2006/01/19 14:45:59 $
 * @since JDK1.1
 */

public class PoleRendering3Dgl extends AnimatedRendering3Dgl {

  public boolean logScale = false;
  public int colrsNumber = 64;
  public ThermalColorMap thermalMap = null;
  public double maxAngle = 90.0;

  public PoleRendering3Dgl(Reflection pole, int drawmode,
                           int resolution, double maxAngle, boolean logScale, int colrsNumber)
//, GraphicsConfiguration g, GLCapabilities glCaps)
  {

    super(pole, drawmode, resolution);
//, g, glCaps);

    String first = new String("Reconstructed pole figure of ");
    first = new String(first + Integer.toString(pole.getH()) + " " +
        Integer.toString(pole.getK()) + " " +
        Integer.toString(pole.getL()));
/*    String log = "";
    if (logScale)
      log = " (log scale)";
    setName(first + log);*/
    this.maxAngle = maxAngle;
    this.logScale = logScale;
    this.colrsNumber = colrsNumber;

  }

  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  public void defineShape(GL2 gl, int mode, int slices) {

    float /*rho,*/ drho;
    float x, y, z;
    float s, t, ds, dt;
    int i, j/*, imin, imax*/;
    boolean normals;
//    float nsign;
//    double xvect[] = null;
    float[] xv = new float[4], yv = new float[4], zv = new float[4], sv = new float[4],
        tv = new float[4], pfv = new float[4];

    int scaleStrain = 1;
    float random = 0.0f;

    normals = false;
//    nsign = 1.0f;

    drho = (float) (2.0 * maxAngle / slices);

    Reflection poleFigure = (Reflection) objectToRender;

    double PF[][] = null;
    if (mode > 0)
      PF = poleFigure.getPoleFigureGrid(slices + 1, maxAngle);
    else {
      PF = poleFigure.getPoleFigureGridStrain(slices + 1, maxAngle);
      mode = -mode;
      random = 1.0f;
      scaleStrain = 100;
    }

/* texturing: s goes from 0.0/0.25/0.5/0.75/1.0 at +y/+x/-y/-x/+y axis */
/* t goes from -1.0/+1.0 at z = -radius/+radius (linear along longitudes) */
/* cannot use triangle fan on texturing (s coord. at top/bottom tip varies) */

    float zmax = 0.0f;
    float zmin = 0.0f;

    for (j = 0; j <= slices; j++)
      for (i = 0; i <= slices; i++) {
        if (logScale && !Double.isNaN(PF[i][j]))
          PF[i][j] = MoreMath.log10(PF[i][j]);
//      		PF[i][j] *= scaleStrain;
//      		PF[i][j] += random;
        if (!Double.isNaN(PF[i][j])) {
          if (zmax < PF[i][j])
            zmax = (float) PF[i][j];
          if (zmin > PF[i][j])
            zmin = (float) PF[i][j];
        }
      }

    float offset = 0.0f;
    if (zmin < 0)
      offset = -zmin;
    for (j = 0; j <= slices; j++)
      for (i = 0; i <= slices; i++) {
        PF[i][j] += offset;
//        System.out.println(PF[i][j]);
      }
    zmax += offset;
    zmin += offset;

    thermalMap = new ThermalColorMap(zmin, zmax, colrsNumber, Constants.grayShaded);


    float scalemax = (zmax - zmin) * scaleStrain;

    if (mode == 1) {
/* draw +Z end as a triangle fan */

      ds = drho; // (float) maxAngle / slices;
      dt = drho; // (float) maxAngle / slices;
      double maxAngle2 = maxAngle * maxAngle;
      t = -(float) maxAngle;  /* because loop now runs from 0 */
      s = -(float) maxAngle;  /* because loop now runs from 0 */
//      imin = 0;
//      imax = slices;

      boolean outsideCircle = false;
      for (j = 0; j < slices; j++) {
        x = j * drho - (float) maxAngle;
        s = -(float) maxAngle;  /* because loop now runs from 0 */
//        gl.glBegin(GL.GL_QUAD_STRIP);
        for (i = 0; i < slices; i++) {
          y = i * drho - (float) maxAngle;
          zv[0] = (float) PF[i][j];
          zv[1] = (float) PF[i][j + 1];
          zv[2] = (float) PF[i + 1][j];
          zv[3] = (float) PF[i + 1][j + 1];
          sv[0] = s;
          sv[1] = s;
          tv[0] = t;
          tv[1] = t + dt;
          xv[0] = x; // tv[0]; // x;
          yv[0] = y; // sv[0]; // y;
          xv[1] = x + drho; // tv[1]; // x + drho;
          yv[1] = y; // sv[1]; // y;
          xv[2] = x; // tv[0]; // x;
          yv[2] = y + drho; // sv[0]; // y;
          xv[3] = x + drho; // tv[1]; // x + drho;
          yv[3] = y + drho; // sv[1]; // y;

          for (int ij = 0; ij < 4; ij++) {
            pfv[ij] = zv[ij];
            zv[ij] *= scaleStrain;
            zv[ij] += random;
            zv[ij] *= 3.0f / scalemax;
          }

          outsideCircle = false;
          for (int ij = 0; ij < 3; ij++) {
            if (Double.isNaN(pfv[ij]))
              outsideCircle = true;
          }
//          outsideCircle = false;
          if (!outsideCircle) {
            gl.glBegin(GL.GL_TRIANGLES);
            for (int ij = 0; ij < 3; ij++) {
              texture(gl, pfv[ij], zmin, zmax);
              gl.glVertex3f(xv[ij], yv[ij], zv[ij] - 3.0f / scalemax);
            }
            gl.glEnd();
          }
          outsideCircle = false;
          for (int ij = 1; ij < 4; ij++) {
            if (Double.isNaN(pfv[ij]))
              outsideCircle = true;
          }
//          outsideCircle = false;
          if (!outsideCircle) {
            gl.glBegin(GL.GL_TRIANGLES);
            for (int ij = 1; ij < 4; ij++) {
              texture(gl, pfv[ij], zmin, zmax);
              gl.glVertex3f(xv[ij], yv[ij], zv[ij] - 3.0f / scalemax);
            }
            gl.glEnd();
          }

          s += ds;

        }
//          ds = -ds;
        t += dt;

//        gl.glEnd();
      }

    } else if (mode == 2) {//(qobj->DrawStyle==GLU_LINE || qobj->DrawStyle==GLU_SILHOUETTE) {
/* draw stack lines */

      for (i = 0; i <= slices; i++) {
        x = i * drho - (float) maxAngle;
        gl.glBegin(GL.GL_LINE_STRIP);
        for (j = 0; j <= slices; j++) {
          y = j * drho - (float) maxAngle;
          z = (float) (PF[i][j] * scaleStrain + random) - 1.0f;
          if (normals)
            gl.glNormal3f(x, y, z);
          gl.glVertex3f(x, y, z);
        }
        gl.glEnd();
      }
/* draw slice lines */
      for (i = 0; i <= slices; i++) {
        y = i * drho - (float) maxAngle;
        gl.glBegin(GL.GL_LINE_STRIP);
        for (j = 0; j <= slices; j++) {
          x = j * drho - (float) maxAngle;
          z = (float) (PF[i][j] * scaleStrain + random) - 1.0f;
          if (x * x + y * y > maxAngle)
            gl.glColor3f(1f, 1f, 1f);
          else
            gl.glColor3f(1f - z, 1f - z, 1f - z);
          if (normals)
            gl.glNormal3f(x, y, z);
          gl.glVertex3f(x, y, z);
        }
        gl.glEnd();
      }
    } else if (mode == 3) {//(qobj->DrawStyle==GLU_POINT) {
/* top and bottom-most points */

/* loop over stacks */
      for (i = 0; i <= slices; i++) {
        x = i * drho - (float) maxAngle;
        for (j = 0; j <= slices; j++) {
          y = j * drho - (float) maxAngle;
          z = (float) PF[i][j] / scalemax;
          gl.glBegin(GL.GL_POINTS);
          if (x * x + y * y > maxAngle)
            gl.glColor3f(1f, 1f, 1f);
          else
            gl.glColor3f(1f - z, 1f - z, 1f - z);
          if (normals)
            gl.glNormal3f(x, y, z * 3f);
          gl.glVertex3f(x, y, z * 3f - 2f);
          gl.glEnd();
        }
      }
    }

  }

  public void texture(GL2 gl, float z, float zmin, float zmax) {
    float[] mat_col = defineColorByIntensity(z, zmin, zmax);
    float[] mat_color = new float[4];
    float[] mat_shin = new float[1];
    for (int i = 0; i < 4; i++)
      mat_color[i] = mat_col[i];
    mat_shin[0] = mat_col[4];
    gl.glMaterialfv(GL.GL_FRONT, GL2.GL_AMBIENT_AND_DIFFUSE, FloatBuffer.wrap(mat_color));
    gl.glMaterialfv(GL.GL_FRONT, GL2.GL_SPECULAR, FloatBuffer.wrap(mat_color));
    gl.glMaterialfv(GL.GL_FRONT, GL2.GL_SHININESS, FloatBuffer.wrap(mat_shin));
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

  public float[] defineColorByIntensity(float z, float zmin, float zmax) {
    Color tmcolor = thermalMap.getColor(z);
    float[] mat_color = new float[5];
    mat_color[0] = tmcolor.getRed() / 255f;
    mat_color[1] = tmcolor.getGreen() / 255f;
    mat_color[2] = tmcolor.getBlue() / 255f;
    mat_color[3] = 1f;
    mat_color[4] = 80.0f;
    return mat_color;
  }

  public static float[] normalToPlaneByPointsBase(float[] xv, float[] yv, float[] zv, int base) {

    float[] normals = new float[3];
    float dy21, dx21, dz21, dy31, dx31, dz31;
    int n1, n2, n3;

    n1 = base;
    n2 = base + 1;
    n3 = base + 2;
    dy21 = yv[n2] - yv[n1];
    if (dy21 == 0) {
      n3 = base;
      n1 = base + 2;
      dy21 = yv[n2] - yv[n1];
    }
    dz21 = zv[n2] - zv[n1];
    dy31 = yv[n3] - yv[n1];
    dz31 = zv[n3] - zv[n1];
    float den = dz21 * dy31 - dz31 * dy21;
    if (den == 0) {
      n2 = base;
      n3 = base + 1;
      dy21 = yv[n2] - yv[n1];
      dz21 = zv[n2] - zv[n1];
      dy31 = yv[n3] - yv[n1];
      dz31 = zv[n3] - zv[n1];
      den = dz21 * dy31 - dz31 * dy21;
    }
    dx31 = xv[n3] - xv[n1];
    dx21 = xv[n2] - xv[n1];

    float b = (dx31 * dy21 - dx21 * dy31) / den;

    float a = -(dx21 + b * dz21) / dy21;

//    float c = -(xv[base] + a * yv[base] + b * zv[base]);

    normals[0] = -1.0f;
    normals[1] = -a;
    normals[2] = -b;

    return normals;

  }

}
