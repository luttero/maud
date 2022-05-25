package it.unitn.ing.rista.mdyn;

import java.lang.*;


/** Transform
 * This class does transformations from one coordinate system
 * to another, allowing rotation, translation and scaling.
 *
 * @version 1.0 (10-Jul-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */


public class Transform
        extends Object
        implements Cloneable {


  // rotation in radiants
  private double rotX,rotY,rotZ;

  // translation values
  private double transX,transY,transZ;

  // scale values
  private double scaleX,scaleY,scaleZ;

  // rotation matrix elements
  private double x1,x2,x3,y1,y2,y3,z1,z2,z3;


  /** Creates a new 3D transformation. Rotation and translation
   * are initialized to 0, scaling to 1
   */

  public Transform() {

    setRotation(0, 0, 0);
    setTranslation(0, 0, 0);
    setScale(1, 1, 1);

  } // end Transform


  /** Sets amount of rotation in each of the three dimensions
   * @param x rotation for the first axis in degrees (full circle is 360)
   * @param y rotation for the second axis in degrees (full circle is 360)
   * @param z rotation for the third axis in degrees (full circle is 360)
   */

  public void setRotation(double x, double y, double z) {

    rotX = (x % 360) * (Math.PI / 180);
    rotY = (y % 360) * (Math.PI / 180);
    rotZ = (z % 360) * (Math.PI / 180);
    // calculate the rotation matrix
    calcMatrix();

  } // end setRotation


  /** Sets amount of translation in each of the three dimensions
   * @param x translation for first axis (0 for no translation)
   * @param y translation for second axis (0 for no translation)
   * @param z translation for third axis (0 for no translation)
   */

  public void setTranslation(double x, double y, double z) {

    transX = x;
    transY = y;
    transZ = z;

  } // end setTranslation


  /** Sets amount of scaling in each of the three dimensions
   * @param x scaling for first axis (1 for no scaling)
   * @param y scaling for second axis (1 for no scaling)
   * @param z scaling for third axis (1 for no scaling)
   */

  public void setScale(double x, double y, double z) {

    scaleX = x;
    scaleY = y;
    scaleZ = z;

  } // end setScale


  /** Transform the given coordinates from their standard space
   * to the world space, which may be rotated, translated or
   * scaled according to the settings of the Transform object
   */

  public void transform(Vector3 src, Vector3 dst) {

    dst.x = scaleX * (x1 * src.x + x2 * src.y + x3 * src.z) + transX;
    dst.y = scaleY * (y1 * src.x + y2 * src.y + y3 * src.z) + transY;
    dst.z = scaleZ * (z1 * src.x + z2 * src.y + z3 * src.z) + transZ;

  } // end transform



  /* Calculate the total rotation matrix, which is a
   * matrix product of the single rotation matrices for each
   * of the three axis'es
   * T = X * Y * Z
   */

  private void calcMatrix() {

    // cache sin/cos values for faster calculation
    double six = Math.sin(rotX);
    double cox = Math.cos(rotX);
    double siy = Math.sin(rotY);
    double coy = Math.cos(rotY);
    double siz = Math.sin(rotZ);
    double coz = Math.cos(rotZ);

    // calculate rotation matrix
    x1 = coy * coz;
    x2 = -coy * siz;
    x3 = siy;
    y1 = six * siy * coz + cox * siz;
    y2 = -six * siy * siz + cox * coz;
    y3 = -six * coy;
    z1 = -cox * siy * coz + six * siz;
    z2 = cox * siy * siz + six * coz;
    z3 = cox * coy;

  } // end calcMatrix


} // end Transform
