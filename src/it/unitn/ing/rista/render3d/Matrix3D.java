/*
 * @(#)Matrix3D.java created 16/11/1998 Berkeley
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

/** A  */

/**
 *  Matrix3D is a fairly conventional 3D matrix object that can transform sets of
 *  3D points and perform a variety of manipulations on the transform.
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:09 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

class Matrix3D {
  double xx, xy, xz, xo;
  double yx, yy, yz, yo;
  double zx, zy, zz, zo;
  static final double pi = 3.14159265;

  /** Create a new unit matrix */
  Matrix3D() {
    xx = 1.0f;
    yy = 1.0f;
    zz = 1.0f;
  }

  /** Scale by f in all dimensions */
  void scale(double f) {
    xx *= f;
    xy *= f;
    xz *= f;
    xo *= f;
    yx *= f;
    yy *= f;
    yz *= f;
    yo *= f;
    zx *= f;
    zy *= f;
    zz *= f;
    zo *= f;
  }

  /** Scale along each axis independently */
  void scale(double xf, double yf, double zf) {
    xx *= xf;
    xy *= xf;
    xz *= xf;
    xo *= xf;
    yx *= yf;
    yy *= yf;
    yz *= yf;
    yo *= yf;
    zx *= zf;
    zy *= zf;
    zz *= zf;
    zo *= zf;
  }

  /** Translate the origin */
  void translate(double x, double y, double z) {
    xo += x;
    yo += y;
    zo += z;
  }

  /** rotate theta degrees about the y axis */
  void yrot(double theta) {
    theta *= (pi / 180);
    double ct = Math.cos(theta);
    double st = Math.sin(theta);

    double Nxx = xx * ct + zx * st;
    double Nxy = xy * ct + zy * st;
    double Nxz = xz * ct + zz * st;
    double Nxo = xo * ct + zo * st;

    double Nzx = zx * ct - xx * st;
    double Nzy = zy * ct - xy * st;
    double Nzz = zz * ct - xz * st;
    double Nzo = zo * ct - xo * st;

    xo = Nxo;
    xx = Nxx;
    xy = Nxy;
    xz = Nxz;
    zo = Nzo;
    zx = Nzx;
    zy = Nzy;
    zz = Nzz;
  }

  /** rotate theta degrees about the x axis */
  void xrot(double theta) {
    theta *= (pi / 180);
    double ct = Math.cos(theta);
    double st = Math.sin(theta);

    double Nyx = yx * ct + zx * st;
    double Nyy = yy * ct + zy * st;
    double Nyz = yz * ct + zz * st;
    double Nyo = yo * ct + zo * st;

    double Nzx = zx * ct - yx * st;
    double Nzy = zy * ct - yy * st;
    double Nzz = zz * ct - yz * st;
    double Nzo = zo * ct - yo * st;

    yo = Nyo;
    yx = Nyx;
    yy = Nyy;
    yz = Nyz;
    zo = Nzo;
    zx = Nzx;
    zy = Nzy;
    zz = Nzz;
  }

  /** rotate theta degrees about the z axis */
  void zrot(double theta) {
    theta *= (pi / 180);
    double ct = Math.cos(theta);
    double st = Math.sin(theta);

    double Nyx = yx * ct + xx * st;
    double Nyy = yy * ct + xy * st;
    double Nyz = yz * ct + xz * st;
    double Nyo = yo * ct + xo * st;

    double Nxx = xx * ct - yx * st;
    double Nxy = xy * ct - yy * st;
    double Nxz = xz * ct - yz * st;
    double Nxo = xo * ct - yo * st;

    yo = Nyo;
    yx = Nyx;
    yy = Nyy;
    yz = Nyz;
    xo = Nxo;
    xx = Nxx;
    xy = Nxy;
    xz = Nxz;
  }

  /** Multiply this matrix by a second: M = M*R */
  void mult(Matrix3D rhs) {
    double lxx = xx * rhs.xx + yx * rhs.xy + zx * rhs.xz;
    double lxy = xy * rhs.xx + yy * rhs.xy + zy * rhs.xz;
    double lxz = xz * rhs.xx + yz * rhs.xy + zz * rhs.xz;
    double lxo = xo * rhs.xx + yo * rhs.xy + zo * rhs.xz + rhs.xo;

    double lyx = xx * rhs.yx + yx * rhs.yy + zx * rhs.yz;
    double lyy = xy * rhs.yx + yy * rhs.yy + zy * rhs.yz;
    double lyz = xz * rhs.yx + yz * rhs.yy + zz * rhs.yz;
    double lyo = xo * rhs.yx + yo * rhs.yy + zo * rhs.yz + rhs.yo;

    double lzx = xx * rhs.zx + yx * rhs.zy + zx * rhs.zz;
    double lzy = xy * rhs.zx + yy * rhs.zy + zy * rhs.zz;
    double lzz = xz * rhs.zx + yz * rhs.zy + zz * rhs.zz;
    double lzo = xo * rhs.zx + yo * rhs.zy + zo * rhs.zz + rhs.zo;

    xx = lxx;
    xy = lxy;
    xz = lxz;
    xo = lxo;

    yx = lyx;
    yy = lyy;
    yz = lyz;
    yo = lyo;

    zx = lzx;
    zy = lzy;
    zz = lzz;
    zo = lzo;
  }

  /** Reinitialize to the unit matrix */
  void unit() {
    xo = 0;
    xx = 1;
    xy = 0;
    xz = 0;
    yo = 0;
    yx = 0;
    yy = 1;
    yz = 0;
    zo = 0;
    zx = 0;
    zy = 0;
    zz = 1;
  }

  /** Transform nvert points from v into tv.  v contains the input
   coordinates in floating point.  Three successive entries in
   the array constitute a point.  tv ends up holding the transformed
   points as integers; three successive entries per point */
  void transform(double v[], int tv[], int nvert) {
    double lxx = xx, lxy = xy, lxz = xz, lxo = xo;
    double lyx = yx, lyy = yy, lyz = yz, lyo = yo;
    double lzx = zx, lzy = zy, lzz = zz, lzo = zo;
    for (int i = nvert * 3; (i -= 3) >= 0;) {
      double x = v[i];
      double y = v[i + 1];
      double z = v[i + 2];
      tv[i] = (int) (x * lxx + y * lxy + z * lxz + lxo);
      tv[i + 1] = (int) (x * lyx + y * lyy + z * lyz + lyo);
      tv[i + 2] = (int) (x * lzx + y * lzy + z * lzz + lzo);
    }
  }

  public String toString() {
    return ("[" + xo + "," + xx + "," + xy + "," + xz + ";"
            + yo + "," + yx + "," + yy + "," + yz + ";"
            + zo + "," + zx + "," + zy + "," + zz + "]");
  }
}
