/*
 * @(#)XYZChemModel.java created 16/11/1998 Berkeley
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

import java.awt.*;

import it.unitn.ing.rista.diffr.*;
import it.unitn.ing.rista.util.*;

import java.awt.image.*;


/**
 *  The representation of a Chemical model.
 *
 *
 * @version $Revision: 1.6 $, $Date: 2004/08/12 09:36:09 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class XYZChemModel {

  double vert[];
  AtomBall atoms[];
  int tvert[];
  int ZsortMap[];
  int nvert, maxvert;

  boolean transformed;
  Matrix3D mat;

  double xmin, xmax, ymin, ymax, zmin, zmax;

  XYZChemModel() {
    mat = new Matrix3D();
    mat.xrot(20);
    mat.yrot(30);
  }

  public void buildtheModel(Phase phase, Component frame, int mode) {

//    double cell[] = Angles.getLattice(phase);

    double ytrasf = phase.getCellValue(1) * MoreMath.cosd(phase.getCellValue(5) - 90.0);
    double ztrasf = phase.getCellValue(2) * MoreMath.cosd(phase.getCellValue(4) - 90.0) *
        MoreMath.cosd(phase.getCellValue(3) - 90.0);

    int numberAtoms = phase.getFullAtomList().size();   // to be ported to the new model
    for (int i = 0; i < numberAtoms; i++) {
      AtomSite anatom = (AtomSite) phase.getFullAtomList().get(i);
      anatom.refreshPositions(true);
      int numPositions = anatom.getSiteMultiplicity();
      for (int ix = 0; ix < numPositions; ix++) {
        double[] x = anatom.getCoordinates(ix);

        addVert(frame, anatom, x[0] * phase.getCellValue(0), x[1] * ytrasf, x[2] * ztrasf);

        if (mode == 1) {
          if (x[0] == 0 && x[1] == 0)
            addVert(frame, anatom, (x[0] + 1.0) * phase.getCellValue(0), (x[1] + 1.0) * ytrasf,
                    x[2] * ztrasf);
          if (x[0] == 0 && x[2] == 0)
            addVert(frame, anatom, (x[0] + 1.0) * phase.getCellValue(0), x[1] * ytrasf,
                    (x[2] + 1.0) * ztrasf);
          if (x[1] == 0 && x[2] == 0)
            addVert(frame, anatom, x[0] * phase.getCellValue(0), (x[1] + 1.0) * ytrasf,
                    (x[2] + 1.0) * ztrasf);
          if (x[0] == 0 && x[1] == 0 && x[2] == 0)
            addVert(frame, anatom, (x[0] + 1.0) * phase.getCellValue(0), (x[1] + 1.0) * ytrasf,
                    (x[2] + 1.0) * ztrasf);
          for (int j = 0; j < 3; j++)
            if (x[j] == 0.0) {
              x[j] += 1.0;
              addVert(frame, anatom, x[0] * phase.getCellValue(0), x[1] * ytrasf, x[2] * ztrasf);
              x[j] -= 1.0;
            }
        }
      }
    }
  }

  /** Add a vertex to this model */
  int addVert(Component frame, AtomSite name, double x, double y, double z) {
    int i = nvert;
    if (i >= maxvert)
      if (vert == null) {
        maxvert = 100;
        vert = new double[maxvert * 3];
        atoms = new AtomBall[maxvert];
      } else {
        maxvert *= 2;
        double nv[] = new double[maxvert * 3];
        System.arraycopy(vert, 0, nv, 0, vert.length);
        vert = nv;
        AtomBall na[] = new AtomBall[maxvert];
        System.arraycopy(atoms, 0, na, 0, atoms.length);
        atoms = na;
      }
    AtomBall a = new AtomBall(name.getMeanWeight(), name.getMeanRadius());
    a.setFrame(frame);
//		if (a == null) a = defaultAtom;
    atoms[i] = a;
    i *= 3;
    vert[i] = x;
    vert[i + 1] = y;
    vert[i + 2] = z;
    return nvert++;
  }

  /** Transform all the points in this model */
  void transform() {
    if (transformed || nvert <= 0)
      return;
    if (tvert == null || tvert.length < nvert * 3)
      tvert = new int[nvert * 3];
    mat.transform(vert, tvert, nvert);
    transformed = true;
  }


  /** Paint this model to a graphics context.  It uses the matrix associated
   with this model to map from model space to screen space.
   The next version of the browser should have double buffering,
   which will make this *much* nicer */
  void paint(Graphics g) {
    if (vert == null || nvert <= 0)
      return;
    transform();
    int v[] = tvert;
    int zs[] = ZsortMap;
    if (zs == null) {
      ZsortMap = zs = new int[nvert];
      for (int i = nvert; --i >= 0;)
        zs[i] = i * 3;
    }

/*
	 * I use a bubble sort since from one iteration to the next, the sort
	 * order is pretty stable, so I just use what I had last time as a
	 * "guess" of the sorted order.  With luck, this reduces O(N log N)
	 * to O(N)
	 */

    for (int i = nvert - 1; --i >= 0;) {
      boolean flipped = false;
      for (int j = 0; j <= i; j++) {
        int a = zs[j];
        int b = zs[j + 1];
        if (v[a + 2] > v[b + 2]) {
          zs[j + 1] = a;
          zs[j] = b;
          flipped = true;
        }
      }
      if (!flipped)
        break;
    }

    int lg = 0;
    int lim = nvert;
    AtomBall ls[] = atoms;
    if (lim <= 0 || nvert <= 0)
      return;
    for (int i = 0; i < lim; i++) {
      int j = zs[i];
      int grey = v[j + 2];
      if (grey < 0)
        grey = 0;
      if (grey > 15)
        grey = 15;
      // g.drawString(names[i], v[j], v[j+1]);
      atoms[j / 3].paint(g, v[j], v[j + 1], grey);
      // g.drawImage(iBall, v[j] - (iBall.width >> 1), v[j + 1] -
      // (iBall.height >> 1));
    }
  }

  /** Find the bounding box of this model */
  void findBB() {
    if (nvert <= 0)
      return;
    double v[] = vert;
    double xmin = v[0], xmax = xmin;
    double ymin = v[1], ymax = ymin;
    double zmin = v[2], zmax = zmin;
    for (int i = nvert * 3; (i -= 3) > 0;) {
      double x = v[i];
      if (x < xmin)
        xmin = x;
      if (x > xmax)
        xmax = x;
      double y = v[i + 1];
      if (y < ymin)
        ymin = y;
      if (y > ymax)
        ymax = y;
      double z = v[i + 2];
      if (z < zmin)
        zmin = z;
      if (z > zmax)
        zmax = z;
    }
    this.xmax = xmax;
    this.xmin = xmin;
    this.ymax = ymax;
    this.ymin = ymin;
    this.zmax = zmax;
    this.zmin = zmin;
  }
}

class AtomBall {
  private Component frame;
  private byte[] data;
  public int R = 80;
  private final static int hx = 15;
  private final static int hy = 15;
  private final static int bgGrey = 192;
  private final static int nBalls = 32;
  private static int maxr;

  private int Rl;
  private int Gl;
  private int Bl;
  private Image balls[];
  private int atomradius = 10;

  void setFrame(Component app) {
    frame = app;
  }

  AtomBall(double atomweight, double radius) {
    int colorintensity = (int) atomweight * 100;
    int blueintensity = (int) atomweight * 66;
    while (colorintensity > 255)
      colorintensity -= 255;
    while (blueintensity > 255)
      blueintensity -= 255;
    Rl = colorintensity;
    Gl = 255 - colorintensity;
    Bl = Math.abs(blueintensity);

    atomradius = (int) (radius * 30);

    data = new byte[R * 2 * R * 2];
    int mr = 0;
    for (int Y = 2 * R; --Y >= 0;) {
      int x0 = (int) (Math.sqrt(R * R - (Y - R) * (Y - R)) + 0.5);
      int p = Y * (R * 2) + R - x0;
      for (int X = -x0; X < x0; X++) {
        int x = X + hx;
        int y = Y - R + hy;
        int r = (int) (Math.sqrt(x * x + y * y) + 0.5);
        if (r > mr)
          mr = r;
        data[p++] = r <= 0 ? 1 : (byte) r;
      }
    }
    maxr = mr;
  }

  private final int blend(int fg, int bg, double fgfactor) {
    return (int) (bg + (fg - bg) * fgfactor);
  }

  private void Setup() {
    balls = new Image[nBalls];
    byte red[] = new byte[256];
    red[0] = (byte) bgGrey;
    byte green[] = new byte[256];
    green[0] = (byte) bgGrey;
    byte blue[] = new byte[256];
    blue[0] = (byte) bgGrey;
    for (int r = 0; r < nBalls; r++) {
      double b = (double) (r + 1) / nBalls;
      for (int i = maxr; i >= 1; --i) {
        double d = (double) i / maxr;
        red[i] = (byte) blend(blend(Rl, 255, d), bgGrey, b);
        green[i] = (byte) blend(blend(Gl, 255, d), bgGrey, b);
        blue[i] = (byte) blend(blend(Bl, 255, d), bgGrey, b);
      }
      IndexColorModel model = new IndexColorModel(8, maxr + 1,
              red, green, blue, 0);
      balls[r] = frame.createImage(
              new MemoryImageSource(R * 2, R * 2, model, data, 0, R * 2));
    }
  }

  void paint(Graphics gc, int x, int y, int r) {
    Image ba[] = balls;
    if (ba == null) {
      Setup();
      ba = balls;
    }
    Image i = ba[r];
    int size = atomradius + r;
    gc.drawImage(i, x - (size >> 1), y - (size >> 1), size, size, frame);
  }
}
