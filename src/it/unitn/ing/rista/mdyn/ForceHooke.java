package it.unitn.ing.rista.mdyn;

import java.lang.*;
import java.util.*;


/**
 * This class implements bond length ("hooke") forces between atoms
 *
 * @version 1.0 (20-Sep-96)
 * @author  Thilo Stoeferle
 * @author  Luca Luterotti
 */

public class ForceHooke
        extends Forces {


  /** Average Bond Length
   */
  public double avgLength = 0;

  // all collected connections
  private Vector conn;

  // hooke constant k
  private double k = 35000; // default: 35000 kJ/mol nm^-2)

  // distance of no force
  private double r0 = 0.152; // default: 0.152 nm


  /** Make necessary initializations
   * @param mol Molecule on which the force is going to act on
   */

  public void initialize(Molecule mol) {

    // get all 2-atom connections
    conn = Forces.collectAtoms(mol, 2);

  } // end initialize


  /** Apply force on molecule
   */

  public void apply() {

    // private potential counter
    double potential = 0;

    // total length of chain
    double totalLen = 0;

    // iterate through all connections
    for (Enumeration en = conn.elements(); en.hasMoreElements();) {

      // get array of connected atoms
      Atom[] atom = (Atom[]) en.nextElement();

      Vector3 dist = Vector3.sub(atom[1].getLoc(), atom[0].getLoc());
      double len = dist.magnitude();
      double delta = len - r0;

      totalLen += len;

      // force F = k * ( r - r0 )
      Vector3 f = Vector3.scale(dist, k * delta / len);
      atom[0].getForce().add(f);
      atom[1].getForce().sub(f);

      // potential U = 1/2 * k * ( r - r0 )^2
      potential += 0.5 * k * delta * delta;

    } // end for

    // set energy
    energy = potential;

    // interaction counter
    interactions = conn.size();

    // set average bond length
    avgLength = totalLen / interactions;

  } // end apply


  /** Set hooke constant
   * @param khooke hooke constant
   */

  public void setHooke(double khooke) {

    k = khooke;

  } // end setHooke


  /** Get hooke constant
   * @return hooke constant
   */

  public double getHooke() {

    return (k);

  } // end getHooke


  /** Set distance of no force
   * @param requal distance of no force
   */

  public void setEqual(double requal) {

    r0 = requal;

  } // end setEqual


  /** Get distance of no force
   * @return distance of no force
   */

  public double getEqual() {

    return (r0);

  } // end getEqual


} // end ForceHooke

