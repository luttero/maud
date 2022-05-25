/*
 * @(#)MapRendering3Djgl.java created Aug 15, 2004 Braila
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
import it.unitn.ing.rista.util.MoreMath;
import it.unitn.ing.rista.util.Constants;

import java.awt.*;

import jgl.*;


/**
 * The MapRendering3Djgl is a class
 * <p/>
 * Description
 *
 * @author Luca Lutterotti
 * @version $Revision: 1.2 $, $Date: 2006/01/19 14:45:59 $
 * @since JDK1.1
 */

public class MapRendering3Djgl extends AnimatedRendering3Djgl {

  private int alphaSlices;
  private int betaSlices;
  private int gammaSlices;
  public boolean logScale = false;
  public int colrsNumber = 64;
  public ThermalColorMap thermalMap = null;
  private double[][][] odf;

  public MapRendering3Djgl(double[][][] odf, int alphaSlices, int betaSlices, int gammaSlices, int drawmode, int resolution, boolean logScale, int colrsNumber)
//, GraphicsConfiguration g, GLCapabilities glCaps)
  {

    super("", null, drawmode, resolution);
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
  public void defineShape(int mode, int slices) {

/* texturing: s goes from 0.0/0.25/0.5/0.75/1.0 at +y/+x/-y/-x/+y axis */
/* t goes from -1.0/+1.0 at z = -radius/+radius (linear along longitudes) */
/* cannot use triangle fan on texturing (s coord. at top/bottom tip varies) */

    float zmax = 0.0f;
    float zmin = 0.0f;

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

    int halfPosAlpha = alphaSlices / 2;
    int halfPosBeta = betaSlices / 2;
    int halfPosGamma = gammaSlices / 2;
    float x1, y1, z1;

    if (mode == 0) {
//(qobj->DrawStyle==GLU_POINT) {
/* top and bottom-most points */

/* loop over stacks */
      gl.glBegin(GL.GL_POINTS);
      for (int i = 0; i < alphaSlices; i++) {
        x1 = (i - halfPosAlpha) / 10.0f;
        for (int j = 0; j < betaSlices; j++) {
          y1 = (j - halfPosBeta) / 10.0f;
          for (int n = 0; n < gammaSlices; n++) {
            z1 = (n - halfPosGamma) / 10.0f;
            float[] colorDensity = defineColorByIntensity((float) odf[i][j][n]);
            gl.glColor3f(colorDensity[0], colorDensity[1], colorDensity[2]);
            gl.glVertex3f(x1, y1, z1);
          }
        }
      }
      gl.glEnd();
    }

  }

  public void texture(float z, float zmin, float zmax) {
    float[] mat_col = defineColorByIntensity(z);
    float[] mat_color = new float[4];
    float[] mat_shin = new float[1];
    for (int i = 0; i < 4; i++)
      mat_color[i] = mat_col[i];
    mat_shin[0] = mat_col[4];
    gl.glMaterialfv(GL.GL_FRONT, GL.GL_AMBIENT_AND_DIFFUSE, mat_color);
    gl.glMaterialfv(GL.GL_FRONT, GL.GL_SPECULAR, mat_color);
    gl.glMaterialfv(GL.GL_FRONT, GL.GL_SHININESS, mat_shin);
  }

  public void textureNotVisible() {
    float[] mat_color = new float[4];
    mat_color[0] = 0;
    mat_color[1] = 0;
    mat_color[2] = 0;
    mat_color[3] = 0;
    float[] mat_shin = new float[1];
    mat_shin[0] = 0;
    gl.glMaterialfv(GL.GL_FRONT, GL.GL_AMBIENT_AND_DIFFUSE, mat_color);
    gl.glMaterialfv(GL.GL_FRONT, GL.GL_SPECULAR, mat_color);
    gl.glMaterialfv(GL.GL_FRONT, GL.GL_SHININESS, mat_shin);
  }

  public float[] defineColorByIntensity(float z) {
    Color tmcolor = thermalMap.getColor(z);
    float[] mat_color = new float[5];
    mat_color[0] = tmcolor.getRed() / 255f;
    mat_color[1] = tmcolor.getGreen() / 255f;
    mat_color[2] = tmcolor.getBlue() / 255f;
    mat_color[3] = 1f;
    mat_color[4] = 80.0f;
    return mat_color;
  }

}
