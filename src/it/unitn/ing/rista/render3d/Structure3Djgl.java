/*
 * @(#)Structure3Djgl.java created Feb 28, 2003 Mesiano
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

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;
import it.unitn.ing.rista.chemistry.AtomInfo;

import java.util.ArrayList;
import java.awt.*;

/**
 *  The Structure3Dgl is a
 *
 *
 * @version $Revision: 1.12 $, $Date: 2004/08/12 09:36:09 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class Structure3Djgl extends AnimatedRendering3Djgl {

	public boolean b_drawCell;
	public boolean b_drawAtoms;
//	public boolean b_StructOnly;
	public boolean b_RotateX;
	public boolean b_RotateY;
	public boolean b_RotateZ;
	float scaleCell = 0f;

//	public boolean plotEntireCell = false;

	float[][] cellVertices = new float[8][3];
	float[] cellCenter = new float[3];

	public Structure3Djgl(StructureModel t_Struct, int width, int height, boolean entireCell) {
		super("Structure", t_Struct, 0, 0, width, height);
		b_drawCell = true;
		b_drawAtoms = true;
	//	b_StructOnly = true;
		b_RotateX = true;
		b_RotateY = true;
		b_RotateZ = true;
	//	plotEntireCell = entireCell;
	}

	public void defineShape(int mode, int slices) {

		updateCell();
    float scaleplot = this.scaleplot * scaleCell;
    gl.glTranslatef(-cellCenter[0], -cellCenter[1], -cellCenter[2]);

    if (b_drawCell)
      drawCell();

    if (!b_drawAtoms)
      return;
		Phase aphase = (Phase) ((XRDcat) objectToRender).getParent();
		aphase.refreshAtoms();
		for (int na = 0; na < aphase.getFullAtomList().size(); na++) {
			AtomSite a_tmp = (AtomSite) aphase.getFullAtomList().get(na);
      if (a_tmp.getOccupancyValue() > 0.0) {
      float atom_color[] = new float[4];
			a_tmp.refreshPositions(true);
			Color atomColor = AtomColorPreferences.getColor(AtomInfo.cutOxidationNumber(a_tmp.getFirstAtomSymbol()));
			atom_color[0] = (float) atomColor.getRed() / 255f;
			atom_color[1] = (float) atomColor.getGreen() / 255f;
			atom_color[2] = (float) atomColor.getBlue() / 255f;
			atom_color[3] = (float) a_tmp.getOccupancyValue();
			ArrayList atomlist = a_tmp.getCartesianCoords(b_drawCell);
			for (int np = 0; np < atomlist.size(); np++) {
				Coordinates abs_c = (Coordinates) atomlist.get(np);
        gl.glTranslatef((float) abs_c.x/scaleplot, (float) abs_c.y/scaleplot, (float) abs_c.z/scaleplot);
        gl.glMaterialfv(gl.GL_FRONT, gl.GL_AMBIENT_AND_DIFFUSE, atom_color);
				gl.glColor4fv(atom_color);
				jgl.glu.GLUquadricObj quadObj = glu.gluNewQuadric();
				glu.gluQuadricDrawStyle(quadObj, glu.GLU_FILL);
				glu.gluQuadricNormals(quadObj, glu.GLU_SMOOTH);
        glu.gluSphere(quadObj, a_tmp.getMeanRadius()/scaleplot, 16, 16);
        gl.glTranslatef((float) -abs_c.x/scaleplot, (float) -abs_c.y/scaleplot, (float) -abs_c.z/scaleplot);
			}
      }
    }
	}

  void updateCell() {

    Phase t_phase = (Phase) ((XRDcat) objectToRender).getParent();

    scaleCell = (float) t_phase.getCellValue(2) / 3;
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

  public void drawCell() {

    int[][] faces = {
      {0, 1, 2, 3},
      {3, 2, 6, 7},
      {7, 6, 5, 4},
      {4, 5, 1, 0},
      {5, 6, 2, 1},
      {7, 4, 0, 3}
    };

    for (int i = 5; i >= 0; i--) {
      gl.glBegin(gl.GL_LINE_LOOP);
      gl.glVertex3fv(cellVertices[faces[i][0]]);
      gl.glVertex3fv(cellVertices[faces[i][1]]);
      gl.glVertex3fv(cellVertices[faces[i][2]]);
      gl.glVertex3fv(cellVertices[faces[i][3]]);
      gl.glEnd();
    }

  }

}
