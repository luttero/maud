package it.unitn.ing.rista.mdyn;

import java.lang.*;


/**
 * This class implements basic operations on 3 dimensional vectors
 *
 * @version 1.0 (10-Jul-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */


public class Vector3
        extends Object
        implements Cloneable {


  /** Coordinates of this 3D point
   */

  public double x,y,z;


  /** Creates a vector
   */

  public Vector3() {
  }


  /** Creates a vector with the same components
   * @param v vector to become cloned
   */

  public Vector3(Vector3 v) {

    x = v.x;
    y = v.y;
    z = v.z;

  } // end Vector3


  /** Creates a vector with initial components
   * @param x initial component
   * @param y initial component
   * @param z initial component
   */

  public Vector3(double x, double y, double z) {

    this.x = x;
    this.y = y;
    this.z = z;

  } // end Vector3


  /** Adds another vector to this vector
   * w = w + v
   * @param v vector to be added
   */

  public void add(Vector3 v) {

    x += v.x;
    y += v.y;
    z += v.z;

  } // end add


  /** Adds one vector to another vector
   * u = v + w
   * @param v first vector
   * @param w second vector to be added
   * @return resulting vector
   */

  static public Vector3 add(Vector3 v, Vector3 w) {

    Vector3 u = new Vector3(v);
    u.add(w);
    return (u);

  } // end add


  /** Substracts another vector from this vector
   * w = w - v
   * @param v vector to be substracted
   */

  public void sub(Vector3 v) {

    x -= v.x;
    y -= v.y;
    z -= v.z;

  } // end sub


  /** Substracts one vector from another vector
   * u = v - w
   * @param v first vector
   * @param w second vector to be subtracted
   * @return resulting vector
   */

  static public Vector3 sub(Vector3 v, Vector3 w) {

    Vector3 u = new Vector3(v);
    u.sub(w);
    return (u);

  } // end sub


  /** Calculates the magnitude (or length) of a vector
   * |v| = sqrt ( x^2 + y^2 + z^2 )
   * @return magnitude of this vector
   */

  public double magnitude() {

    return (Math.sqrt(x * x + y * y + z * z));

  } // end magnitude


  /** Scales the vector by a scalar value
   * v = r * v
   * @param r scalar value
   */

  public void scale(double r) {

    x *= r;
    y *= r;
    z *= r;

  } // end scale


  /** Scales a vector by a scalar value
   * u = r * v
   * @param r scalar value
   * @return resulting vector
   */

  static public Vector3 scale(Vector3 v, double r) {

    Vector3 u = new Vector3(v);
    u.scale(r);
    return (u);

  } // end scale


  /** Calculates the scalar product of two vectors ( A * B )
   * u = vx*wx + vy*wy + vz*wz
   * @param v first vector
   * @param w second vector
   * @return scalar product of the two vectors
   */

  static public double scalarProduct(Vector3 v, Vector3 w) {

    return (v.x * w.x + v.y * w.y + v.z * w.z);

  } // end scalarProduct


  /** Calculates the cross product of two vectors ( A x B )
   * u = ( vy*wz-vz*wy , vz*wx-vx*wz , vx*wy-vy*wx )
   * @param v first vector
   * @param w second vector
   */

  static public Vector3 crossProduct(Vector3 v, Vector3 w) {

    return (new Vector3(v.y * w.z - v.z * w.y,
            v.z * w.x - v.x * w.z,
            v.x * w.y - v.y * w.x));

  } // end crossProduct


  /** Creates a string representation of this vector
   * @return a string representation of this vector
   */

  public String toString() {

    return ("( " + x + " , " + y + " , " + z + " )");

  } // end toXRDcatString


} // end Vector3
