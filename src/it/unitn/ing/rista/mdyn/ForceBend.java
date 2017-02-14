package it.unitn.ing.rista.mdyn;

import java.lang.*;
import java.util.*;


/**
 * This class implements bond angle ("bend") forces between atoms
 *
 * @version 1.0 (20-Sep-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

public class ForceBend
        extends Forces {


  /** Average Bond Angle
   */
  public double avgAngle = 0;

  // all collected connections
  private Vector conn;

  // bend force constant k
  private double k = 1500; // default: 1500 kJ/mol nm

  // angle of no bend force
  private double costheta0 = -0.3333; // default: 109 deg


  /** Make necessary initializations
   * @param mol Molecule on which the force is going to act on
   */

  public void initialize(Molecule mol) {

    // get all 3-atom connections
    conn = Forces.collectAtoms(mol, 3);

  } // end initialize


  /** Apply force on molecule
   */

  public void apply() {

    // private potential counter
    double potential = 0;

    // all angles
    double totalCos = 0;

    // iterate through all connections
    for (Enumeration en = conn.elements(); en.hasMoreElements();) {

      // get array of connected atoms
      Atom[] atom = (Atom[]) en.nextElement();

      Vector3 dist1 = Vector3.sub(atom[0].getLoc(), atom[1].getLoc());
      Vector3 dist2 = Vector3.sub(atom[2].getLoc(), atom[1].getLoc());

      double rlen1 = 1.0 / dist1.magnitude();
      double rlen2 = 1.0 / dist2.magnitude();
      double rlen12 = rlen1 * rlen2;
      double costheta = Vector3.scalarProduct(dist1, dist2) * rlen12;
      double rlen11costheta = costheta * rlen1 * rlen1;
      double rlen22costheta = costheta * rlen2 * rlen2;
      double deltacos = costheta - costheta0;
      double dUdcostheta = k * deltacos;

      totalCos += costheta;

      // force
      Vector3 dt1 = new Vector3(
              -dUdcostheta * (rlen12 * dist2.x - rlen11costheta * dist1.x),
              -dUdcostheta * (rlen12 * dist2.y - rlen11costheta * dist1.y),
              -dUdcostheta * (rlen12 * dist2.z - rlen11costheta * dist1.z));
      Vector3 dt2 = new Vector3(
              -dUdcostheta * (rlen12 * dist1.x - rlen22costheta * dist2.x),
              -dUdcostheta * (rlen12 * dist1.y - rlen22costheta * dist2.y),
              -dUdcostheta * (rlen12 * dist1.z - rlen22costheta * dist2.z));
      atom[0].getForce().add(dt1);
      atom[1].getForce().sub(dt1);
      atom[1].getForce().sub(dt2);
      atom[2].getForce().add(dt2);

      // potential U = 1/2 * k * ( cos theta - cos theta0 )^2
      potential += 0.5 * dUdcostheta * deltacos;

    } // end for

    // set energy
    energy = potential;

    // interaction counter
    interactions = conn.size();

    // set average angle
    avgAngle = Math.acos(totalCos / interactions) * 180 / Math.PI;

  } // end apply


  /** Set bend force constant
   * @param kbend bend force constant
   */

  public void setBend(double kbend) {

    k = kbend;

  } // end setBend


  /** Get bend force constant
   * @return bend force constant
   */

  public double getBend() {

    return (k);

  } // end getBend


  /** Set cosine of angle of no force
   * @param costhetaequal cosine of angle of no force
   */

  public void setEqualCos(double costhetaequal) {

    costheta0 = costhetaequal;

  } // end setEqualCos


  /** Get cosine of angle of no force
   * @return cosine of angle of no force
   */

  public double getEqualCos() {

    return (costheta0);

  } // end getEqualCos


  /** Set angle of no force
   * @param thetaequal angle of no force (in degrees)
   */

  public void setEqualAngle(double thetaequal) {

    costheta0 = Math.cos(Math.PI * thetaequal / 180);

  } // end setEqualAngle


  /** Get cosine of angle of no force
   * @return angle of no force (in degrees)
   */

  public double getEqualAngle() {

    return (Math.acos(costheta0) * 180 / Math.PI);

  } // end getEqualAngle


} // end ForceBend

