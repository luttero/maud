/*
 * @(#)Structure3Dgl.java created Feb 26, 2003 Mesiano
 *
 * Copyright (c) 2003 Luca Lutterotti All Rights Reserved.
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

import it.unitn.ing.rista.chemistry.AtomInfo;
import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import javax.media.opengl.*;
import javax.media.opengl.glu.*;
import java.awt.*;
import java.util.*;
import java.nio.FloatBuffer;

/**
 *  The Structure3Dgl is a
 *
 *
 * @version $Revision: 1.21 $, $Date: 2006/01/19 14:45:59 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Structure3Dgl extends AnimatedRendering3Dgl {
//	private StructureModel m_Struct = null;

  public boolean b_drawCell;
  public boolean b_drawAtoms;
  public boolean b_RotateX;
  public boolean b_RotateY;
  public boolean b_RotateZ;
  float scaleCell = 0f;
  int numberOfAtoms = 0;
  float[][] atomCharToDraw = null;
  float[][] atomColor = null;

//	public boolean plotEntireCell = false;

  static float[][] cellVertices = new float[8][3];
  static float[] cellCenter = new float[3];

  public Structure3Dgl(StructureModel t_Struct, int width, int height, boolean entireCell) {
    super(t_Struct, 0, 50, width, height);
    //super(width, height);
//		this.m_Struct = t_Struct;
    b_drawCell = true;
    b_drawAtoms = true;
    b_RotateX = true;
    b_RotateY = true;
    b_RotateZ = true;
//		plotEntireCell = entireCell;
  }

  public void drawStructure(GL2 gl) {

    gl.glTranslatef(-cellCenter[0], -cellCenter[1], -cellCenter[2]);

    if (b_drawCell)
      drawCell(gl);

    if (b_drawAtoms)
    for (int i = 0; i < numberOfAtoms; i++) {
      gl.glTranslatef(atomCharToDraw[i][0], atomCharToDraw[i][1], atomCharToDraw[i][2]);
      gl.glMaterialfv(GL.GL_FRONT, GL2.GL_AMBIENT_AND_DIFFUSE, FloatBuffer.wrap(atomColor[i]));
        gl.glColor4fv(FloatBuffer.wrap(atomColor[i]));
        GLUquadric quadObj = glu.gluNewQuadric();
        glu.gluQuadricDrawStyle(quadObj, GLU.GLU_FILL);
        glu.gluQuadricNormals(quadObj, GLU.GLU_SMOOTH);
        glu.gluSphere(quadObj, atomCharToDraw[i][3], 16, 16);
        gl.glTranslatef(-atomCharToDraw[i][0], -atomCharToDraw[i][1], -atomCharToDraw[i][2]);

    }
  }

  public void defineShape(GL gl, int mode, int slices) {

    updateCell();
    Vector allAtoms = new Vector(0, 1);

    float scaleplot = this.scaleplot * scaleCell;

    Phase aphase = (Phase) ((XRDcat) objectToRender).getParent();

    aphase.refreshAtoms();
    for (int na = 0; na < aphase.getFullAtomList().size(); na++) {
      AtomSite a_tmp = (AtomSite) aphase.getFullAtomList().get(na);
      float atom_color[] = new float[4];
//      System.out.println(AtomInfo.cutOxidationNumber(a_tmp.getAtomSymbol()));
      a_tmp.refreshPositions(true);
      Color atomColor = AtomColorPreferences.getColor(AtomInfo.cutOxidationNumber(a_tmp.getFirstAtomSymbol()));
      atom_color[0] = (float) atomColor.getRed() / 255f;
      atom_color[1] = (float) atomColor.getGreen() / 255f;
      atom_color[2] = (float) atomColor.getBlue() / 255f;
      atom_color[3] = 1.0f;
      ArrayList atomlist = a_tmp.getCartesianCoords(b_drawCell);
      for (int np = 0; np < atomlist.size(); np++) {
        Coordinates abs_c = (Coordinates) atomlist.get(np);
        float[] atomChar = new float[4];
        float[] atomcolor = new float[4];
        for (int i = 0; i < 4; i++)
          atomcolor[i] = atom_color[i];
        atomChar[0] = (float) abs_c.x/scaleplot;
        atomChar[1] = (float) abs_c.y/scaleplot;
        atomChar[2] = (float) abs_c.z/scaleplot;
        atomChar[3] = (float) a_tmp.getMeanRadius()/scaleplot;
        allAtoms.add(atom_color);
        allAtoms.add(atomChar);
      }
    }
    numberOfAtoms = allAtoms.size() / 2;
    atomCharToDraw = new float[numberOfAtoms][4];
    atomColor = new float[numberOfAtoms][4];
    int index = 0;
    for (int i = 0; i < numberOfAtoms; i++) {
      float[] atomcolor = (float[]) allAtoms.get(index++);
      float[] atomChar = (float[]) allAtoms.get(index++);
      for (int j = 0; j < 4; j++)
        atomCharToDraw[i][j] = atomChar[j];
      for (int j = 0; j < 4; j++)
        atomColor[i][j] = atomcolor[j];
    }
  }

  void updateCell() {

    Phase t_phase = (Phase) ((XRDcat) objectToRender).getParent();

    scaleCell = (float) t_phase.getCellValue(2) / 2;
    //gl.glScalef(1 / scaleCell / scaleplot, 1 / scaleCell / scaleplot, 1 / scaleCell / scaleplot);

    //gl.glScalef(1/3, 1/3, 1/3);

    float thescale = scaleCell * scaleplot;

    float cell_a = (float) t_phase.getCellValue(0) / thescale;
    float cell_b = (float) t_phase.getCellValue(1) / thescale;
    float cell_c = (float) t_phase.getCellValue(2) / thescale;
    float cosalpha = (float) Math.cos(t_phase.getCellValue(3) * Constants.DEGTOPI);
    float cosbeta = (float) Math.cos(t_phase.getCellValue(4) * Constants.DEGTOPI);
    float cosgamma = (float) Math.cos(t_phase.getCellValue(5) * Constants.DEGTOPI);
    float singamma = (float) Math.sin(t_phase.getCellValue(5) * Constants.DEGTOPI);
    float V = (float) t_phase.getCellVolume() / thescale / thescale / thescale;

    // find cell center

    cellCenter[0] = (cell_a + cell_b * cosgamma + cell_c * cosbeta) / 2;
    cellCenter[1] = (cell_b * singamma + cell_c * (cosalpha - cosbeta * cosgamma) / singamma) / 2;
    cellCenter[2] = (V / (cell_a * cell_b * singamma)) / 2;


    // calculates real cartesian positions of cell vertices:
    // (0,0,0), (1,0,0), (1,1,0), (0,1,0), (0,0,1), (1,0,1), (1,1,1), (0,1,1)


    cellVertices = new float[8][3];

    cellVertices[0][0] = 0;
    cellVertices[0][1] = 0;
    cellVertices[0][2] = 0;

    cellVertices[1][0] = cell_a;
    cellVertices[1][1] = 0;
    cellVertices[1][2] = 0;

    cellVertices[2][0] = cell_a + cell_b * cosgamma;
    cellVertices[2][1] = cell_b * singamma;
    cellVertices[2][2] = 0;

    cellVertices[3][0] = cell_b * cosgamma;
    cellVertices[3][1] = cell_b * singamma;
    cellVertices[3][2] = 0;

    cellVertices[4][0] = cell_c * cosbeta;
    cellVertices[4][1] = cell_c * (cosalpha - cosbeta * cosgamma) / singamma;
    cellVertices[4][2] = V / (cell_a * cell_b * singamma);

    cellVertices[5][0] = cell_a + cell_c * cosbeta;
    cellVertices[5][1] = cell_c * (cosalpha - cosbeta * cosgamma) / singamma;
    cellVertices[5][2] = V / (cell_a * cell_b * singamma);

    cellVertices[6][0] = cell_a + cell_b * cosgamma + cell_c * cosbeta;
    cellVertices[6][1] = cell_b * singamma + cell_c * (cosalpha - cosbeta * cosgamma) / singamma;
    cellVertices[6][2] = V / (cell_a * cell_b * singamma);

    cellVertices[7][0] = cell_b * cosgamma + cell_c * cosbeta;
    cellVertices[7][1] = cell_b * singamma + cell_c * (cosalpha - cosbeta * cosgamma) / singamma;
    cellVertices[7][2] = V / (cell_a * cell_b * singamma);


  }

  public void drawCell(GL2 gl) {

    int[][] faces = {
      {0, 1, 2, 3},
      {3, 2, 6, 7},
      {7, 6, 5, 4},
      {4, 5, 1, 0},
      {5, 6, 2, 1},
      {7, 4, 0, 3}
    };

    for (int i = 5; i >= 0; i--) {
      gl.glBegin(GL.GL_LINE_LOOP);
      gl.glVertex3fv(FloatBuffer.wrap(cellVertices[faces[i][0]]));
      gl.glVertex3fv(FloatBuffer.wrap(cellVertices[faces[i][1]]));
      gl.glVertex3fv(FloatBuffer.wrap(cellVertices[faces[i][2]]));
      gl.glVertex3fv(FloatBuffer.wrap(cellVertices[faces[i][3]]));
      gl.glEnd();
    }

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
//    gl.glNewList(objectID, gl.GL_COMPILE); /* create display list */
//    System.out.println("shape");
    defineShape(gl, drawmode, /* slices */ resolution);
//    System.out.println("endlist");
//    gl.glEndList();

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
